module Api.Fs
  ( makeNotebook
  , deleteItem
  , makeFile
  , moveItem
  , listing
  ) where

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head, last)
import Data.Either (Either(..), either)
import Data.Foreign.Class (IsForeign, readProp, read)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String (split)
import Model.File.Item (Item(), itemPath)
import Model.File.Resource (Resource(..))
import Model.Notebook (Notebook())
import Network.HTTP.Affjax (Affjax(), AJAX(), affjax, defaultRequest, get, put_, delete_)
import Network.HTTP.Affjax.Response (Respondable, ResponseType(..))
import Network.HTTP.Method (Method(MOVE))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import qualified Data.String.Regex as Rgx

newtype Listing = Listing [Child]

instance listingIsForeign :: IsForeign Listing where
  read f = Listing <$> readProp "children" f

newtype Child = Child { name :: String, resource :: Resource }

instance childIsForeign :: IsForeign Child where
  read f = Child <$>
    ({name: _, resource: _} <$>
     readProp "name" f <*>
     readProp "type" f)

instance listingRespondable :: Respondable Listing where
  responseType = JSONResponse
  fromResponse = read

listing2items :: Listing -> [Item]
listing2items (Listing cs) =
  child2item <$> cs
  where nbExtensionRgx = Rgx.regex ("\\" <> Config.notebookExtension) Rgx.noFlags
        isNotebook r = r.resource == File &&
                       r.name /=
                       Rgx.replace nbExtensionRgx "" r.name
        child2item (Child r) =
          let item = {
                resource: r.resource,
                name: r.name,
                selected: false,
                hovered: false,
                phantom: false,
                root: ""
                }
          in if isNotebook r then
               item{resource = Notebook}
             else item


successStatus :: StatusCode
successStatus = StatusCode $ fromNumber 200

succeeded :: StatusCode -> Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = toNumber int

getResponse :: forall e a. String -> Affjax e a -> Aff (ajax :: AJAX | e) a
getResponse msg affjax = do
  res <- affjax
  if not $ succeeded res.status
    then throwError $ error msg
    else
    pure res.response

listing' :: forall e. String -> Affjax e Listing
listing' path = get (Config.metadataUrl <> path)

listing :: forall e. String -> Aff (ajax :: AJAX | e) [Item]
listing path = (listing2items <<< _.response) <$> listing' path

makeFile :: forall e. Item -> String -> Aff (ajax :: AJAX | e) Unit
makeFile item content =
  let path = itemPath item
      isJson = either (const false) (const true) do
        hd <- maybe (Left "empty file") Right $
              head $ split "\n" content
        jsonParser hd
  in if isJson then do
    getResponse ("error while creating file " <> path) $
    put_ (Config.dataUrl <> path) content
    else throwError $ error "file has incorrect format"

makeNotebook :: forall e. Item -> Notebook -> Aff (ajax :: AJAX | e) Unit
makeNotebook item notebook =
  let path = itemPath item in
  getResponse ("error while creating notebook " <> path) $
  put_ (Config.dataUrl <> path) notebook

delete :: forall e. String -> Aff (ajax :: AJAX | e) Unit
delete path = getResponse ("can not delete " <> path) $
              delete_ (Config.dataUrl <> path)

deleteItem :: forall e. Item -> Aff (ajax :: AJAX | e) Unit
deleteItem item =
    let path = itemPath item
    in delete path

moveItem :: forall e. Item -> String -> Aff (ajax :: AJAX | e) String
moveItem item destination = do
  let path = Config.dataUrl <> itemPath item
      dest = if item.resource == Notebook
             then case last $ split "." destination of
               Just "slam" -> destination
               Just _ -> destination <> ".slam"
               Nothing -> destination
             else destination
      dest' = case item.resource of
        Directory -> dest <> "/"
        Database -> dest <> "/"
        _ -> dest
  result <- affjax $ defaultRequest {
    method = MOVE,
    headers = [RequestHeader "Destination" dest'],
    url = path
    }
  if succeeded result.status
    then pure ""
    else pure result.response
