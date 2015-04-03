-- | This module will be reworked after `affjax` is ready.
module Api.Fs (
  makeNotebook,
  deleteItem,
  makeFile,
  listing
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Exception (error)
import DOM (DOM())
import Data.Maybe
import Data.Either
import Control.Monad.Error.Class
import Data.Foreign
import Data.Foreign.Class
import Data.Int

import qualified Control.Monad.Aff as Aff
import qualified Data.Array as Arr
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Data.Argonaut.Parser as Ap
import qualified Network.HTTP.Affjax.Response as Ar
import qualified Network.HTTP.Affjax.ResponseType as At
import qualified Network.HTTP.Affjax as Af
import qualified Network.HTTP.StatusCode as Sc

import qualified Config as Config
import qualified Model as M
import qualified Model.Item as Mi
import qualified Model.Resource as Mr
import qualified Model.Notebook as Mn


newtype Listing = Listing [Child]

instance listingIsForeign :: IsForeign Listing where
  read f = Listing <$> readProp "children" f

newtype Child = Child {name :: String, resource :: Mr.Resource}

instance childIsForeign :: IsForeign Child where
  read f = Child <$> 
    ({name: _, resource: _} <$>
     readProp "name" f <*>
     readProp "type" f)


instance listingResponsable :: Ar.Responsable Listing where
  responseType _ = At.JSONResponse
  fromResponse = read

listing2items :: Listing -> [Mi.Item]
listing2items (Listing cs) =
  child2item <$> cs
  where nbExtensionRgx = Rgx.regex ("\\" <> Config.notebookExtension) Rgx.noFlags
        isNotebook r = r.resource == Mr.File &&
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
               item{resource = Mr.Notebook}
             else item


successStatus :: Sc.StatusCode
successStatus = Sc.StatusCode $ fromNumber 200

getResponse :: forall e a. String -> Af.Affjax e a -> Aff.Aff (ajax::Af.Ajax|e) a 
getResponse msg affjax = do
  res <- affjax
  if res.status /= successStatus then
    throwError $ error msg
    else
    pure res.response

listing' :: forall e. String -> Af.Affjax e Listing
listing' path = Af.get (Config.metadataUrl <> path)

listing :: forall e. String -> Aff.Aff (ajax::Af.Ajax|e) [Mi.Item]
listing path = (listing2items <<< _.response) <$> listing' path

makeFile :: forall e. Mi.Item -> String -> Aff.Aff (ajax::Af.Ajax|e) Unit
makeFile item content =
  let path = Mi.itemPath item
      isJson = either (const false) (const true) do
        hd <- maybe (Left "empty file") Right $
              Arr.head $ Str.split "\n" content
        Ap.jsonParser hd
  in if isJson then do
    getResponse ("error while creating file " <> path) $
    Af.put_ (Config.dataUrl <> path) content
    else throwError $ error "file has incorrect format" 
                        
makeNotebook :: forall e. Mi.Item -> Mn.Notebook -> Aff.Aff (ajax::Af.Ajax|e) Unit 
makeNotebook item notebook =
  let path = Mi.itemPath item in
  getResponse ("error while creating notebook " <> path) $ 
  Af.put_ (Config.dataUrl <> path) notebook

delete :: forall e. String -> Aff.Aff (ajax::Af.Ajax|e) Unit
delete path = getResponse ("can not delete " <> path) $ 
              Af.delete_ (Config.dataUrl <> path)

deleteItem :: forall e. Mi.Item -> Aff.Aff (ajax :: Af.Ajax|e) Unit
deleteItem item =
    let path = Mi.itemPath item <>
               if item.resource /= Mr.File &&
                  item.resource /= Mr.Notebook then "/"
               else ""
    in delete path 
