module Api.Fs where

import Control.Monad.Aff(Aff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.Maybe

import Data.Array (head)
import Data.String (split)
import Data.Foreign.Class (readProp, read, IsForeign)
import Network.HTTP.Affjax.Response (Respondable, ResponseType(JSONResponse))
import Network.HTTP.Affjax (Affjax(), AJAX(), affjax, get, put_, delete_, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.Method (Method(MOVE))
import Data.Argonaut.Parser (jsonParser)

import Api.Common (succeeded, getResponse)
import Model.Notebook.Domain (Notebook())
import Model.Resource (getPath, Resource(), resourcePath, _root, AnyPath(), newNotebook, newFile, _path)
import Data.Path.Pathy (rootDir)
import Optic.Core ((.~))
import Config

newtype Listing = Listing [Resource]

runListing :: Listing -> [Resource]
runListing (Listing rs) = rs

instance listingIsForeign :: IsForeign Listing where
  read f = Listing <$> readProp "children" f

instance listingRespondable :: Respondable Listing where
  responseType = JSONResponse
  fromResponse = read

children :: forall e. Resource -> Aff (ajax :: AJAX | e) [Resource] 
children r = do
  cs <- children' $ resourcePath r 
  pure $ (_root .~ (either (const rootDir) id $ getPath r)) <$> cs 
  where
  msg = "error getting resource children"

  children' :: String -> Aff _ [Resource]
  children' str = runListing <$> (getResponse msg $ listing str)

  listing :: String -> Affjax _ Listing
  listing str = get (Config.metadataUrl <> str)

makeFile :: forall e. AnyPath -> String -> Aff (ajax :: AJAX | e) Unit
makeFile ap content =
  getResponse msg $ either err go isJson
  where
  resource :: Resource
  resource = newFile # _path .~ ap 

  msg :: String
  msg = "error while creating file"

  err :: _ -> Aff _ _
  err _ = throwError $ error "file has incorrect format"

  firstLine :: Maybe String
  firstLine = head $ split "\n" content

  isJson :: Either _ _
  isJson = maybe (Left "empty file") Right firstLine >>= jsonParser

  go :: _ -> Aff _ _
  go _ = put_ (Config.dataUrl <> resourcePath resource) content


makeNotebook :: forall e. AnyPath -> Notebook -> Aff (ajax :: AJAX | e) Unit
makeNotebook ap notebook =
  getResponse msg $ put_ (Config.dataUrl <> resourcePath resource <> "/index") notebook
  where msg = "error while creating notebook"
        resource = newNotebook # _path .~ ap 

delete :: forall e. Resource -> Aff (ajax :: AJAX | e) Unit
delete resource =
  getResponse msg $ delete_ (Config.dataUrl <> resourcePath resource)
  where msg = "can not delete"


move :: forall e. Resource -> AnyPath -> Aff (ajax :: AJAX | e) (Either String AnyPath)
move src tgt = do
  result <- affjax $ defaultRequest
    { method = MOVE
    , headers = [RequestHeader "Destination" $ resourcePath (src # _path .~ tgt) ]
    , url = Config.dataUrl <> resourcePath src 
    }
  pure if succeeded result.status
       then Right tgt
       else Left result.response
