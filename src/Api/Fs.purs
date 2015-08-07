module Api.Fs where

import Prelude
import Api.Common (RetryEffects(), succeeded, getResponse, retryGet, retryDelete, retryPost, retryPut, slamjax)
import Control.Apply ((*>))
import Control.Bind ((>=>))
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json())
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head, tail, findIndex, filter, elemIndex, (:))
import Data.Either (Either(..), either)
import Data.Foreign (Foreign(), F(), parseJSON)
import Data.Foreign.Index (prop)
import Data.Foreign.Class (readProp, read, IsForeign)
import Data.Maybe
import Data.Bifunctor (bimap)
import Data.Path.Pathy
import Data.These (These(..), theseLeft, theseRight)
import Data.Tuple (Tuple(..))
import Model.Path
import Model.Notebook.Cell
import Model.Notebook.Port
import Network.HTTP.Affjax (Affjax(), AJAX(), affjax, defaultRequest)
import Network.HTTP.Affjax.Response (Respondable, ResponseType(JSONResponse))
import Network.HTTP.Method (Method(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.MimeType (MimeType())
import Network.HTTP.MimeType.Common (applicationJSON)
import Optic.Core

import qualified Data.Maybe.Unsafe as U
import qualified Data.String as S
import qualified Model.Notebook.Domain as N
import qualified Model.Resource as R
import Model.Notebook.Port (_PortResource)

newtype Listing = Listing (Array R.Resource)

runListing :: Listing -> Array R.Resource
runListing (Listing rs) = rs

instance listingIsForeign :: IsForeign Listing where
  read f = Listing <$> readProp "children" f

instance listingRespondable :: Respondable Listing where
  responseType = JSONResponse
  fromResponse = read

children :: forall e. DirPath -> Aff (RetryEffects (ajax :: AJAX | e)) (Array R.Resource)
children dir = do
  cs <- children' $ printPath dir
  pure $ (R._root .~ (either (const rootDir) id (Right dir))) <$> cs

children' :: forall e. String -> Aff (RetryEffects (ajax :: AJAX | e)) (Array R.Resource)
children' str = runListing <$> (getResponse msg $ listing str)
  where
  msg = "error getting resource children"

listing :: forall e. String -> Affjax (RetryEffects e) Listing
listing str = retryGet $ Config.metadataUrl <> str

makeFile :: forall e. AnyPath -> Maybe MimeType -> String -> Aff (RetryEffects (ajax :: AJAX | e)) Unit
makeFile ap mime content =
  getResponse msg $ go unit
  where
  resource :: R.Resource
  resource = R.newFile # R._path .~ ap

  msg :: String
  msg = "error while creating file"

  err :: _ -> Aff _ _
  err _ = throwError $ error "file has incorrect format"

  firstLine :: Maybe String
  firstLine = head $ S.split "\n" content

  isJson :: Either _ _
  isJson = maybe (Left "empty file") Right firstLine >>= jsonParser

  go :: _ -> Aff _ _
  go _ = slamjax $ defaultRequest
    { method = PUT
    , headers = maybe [] (pure <<< ContentType) mime
    , content = Just content
    , url = Config.dataUrl <> R.resourcePath resource
    }

loadNotebook :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) N.Notebook
loadNotebook res = do
  val <- getResponse "error loading notebook" $ retryGet (Config.dataUrl <> R.resourcePath res <> "/index")
  case decodeJson (foreignToJson val) of
    Left err -> throwError (error err)
    Right notebook ->
      let name = dropNotebookExt (R.resourceName res)
          path = R.resourceDir res
          nPath = either (const rootDir) id (R.getPath res)
      in pure $  (notebook # (N._path .~ R.resourceDir res)
                  .. (N._name .~ That name)
                  .. (N.syncCellsOuts nPath)
                 )


-- TODO: Not this. either add to Argonaut, or make a Respondable Json instance (requires "argonaut core" - https://github.com/slamdata/purescript-affjax/issues/16#issuecomment-93565447)
foreign import foreignToJson :: Foreign -> Json

-- | Saves (creating or updating) a notebook. If the notebook's `name` value is
-- | a `This` value the name will be used as a basis for generating a new
-- | notebook. If the `name` value is a `Both` value the notebook will be saved
-- | and then moved. If the name is a `That` the notebook will be saved.
saveNotebook :: forall e. N.Notebook -> Aff (RetryEffects (ajax :: AJAX | e)) N.Notebook
saveNotebook notebook = case notebook ^. N._name of
  That name -> save name notebook *> pure notebook
  This name -> do
    name <- getNewName' (U.fromJust $ theseLeft (notebook ^. N._name))
    let notebook' = N.replacePendingPorts (
          notebook # (N._name .~ That (dropNotebookExt name))
                   ..(N._cells .. mapped .. _hasRun .~ false)
          )
    save name notebook'
    pure notebook'
  Both newName oldName -> do
    save oldName notebook
    if newName /= oldName
      then do
      newName' <- getNewName' newName
      let oldPath = (notebook ^. N._path) </> dir oldName <./> Config.notebookExtension
          path = (notebook ^. N._path) </> dir newName' <./> Config.notebookExtension
          newPath = Right $ path
      move (R.Directory oldPath) newPath
      pure (notebook # (N._name .~ That (dropNotebookExt newName'))
                    .. (N.syncCellsOuts path))

      else pure notebook
  where

  getNewName' :: String -> Aff (RetryEffects (ajax :: AJAX | e)) String
  getNewName' name =
    let baseName = name ++ "." ++ Config.notebookExtension
    in getNewName (notebook ^. N._path) baseName

  save :: String -> N.Notebook -> Aff (RetryEffects (ajax :: AJAX | e)) Unit
  save name notebook =
    let notebookPath = (notebook ^. N._path) </> dir name <./> Config.notebookExtension </> file "index"
    in getResponse "error while saving notebook" $ retryPut (Config.dataUrl <> printPath notebookPath) notebook

-- | Generates a new resource name based on a directory path and a name for the
-- | resource. If the name already exists in the path a number is appended to
-- | the end of the name.
getNewName :: forall e. DirPath -> String -> Aff (RetryEffects (ajax :: AJAX | e)) String
getNewName parent name = do
  items <- children' (printPath parent)
  pure if exists' name items then getNewName' items 1 else name
  where
  getNewName' items i =
    let arr = S.split "." name
    in fromMaybe "" do
      body <- head arr
      suffixes <- tail arr
      let newName = S.joinWith "." $ (body <> " " <> show i):suffixes
      pure if exists' newName items
           then getNewName' items (i + 1)
           else newName

exists :: forall e. String -> DirPath -> Aff (RetryEffects (ajax :: AJAX | e)) Boolean
exists name parent = exists' name <$> children' (printPath parent)

exists' :: forall e. String -> Array R.Resource -> Boolean
exists' name items = isJust $ findIndex (\r -> r ^. R._name == name) items

forceDelete :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) Unit
forceDelete resource =
  getResponse msg $ retryDelete path
  where
  msg :: String
  msg = "can not delete"

  path = (if R.isDatabase resource then Config.mountUrl else Config.dataUrl)
         <> R.resourcePath resource

delete :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) (Maybe R.Resource)
delete resource =
  if not (R.isDatabase resource || alreadyInTrash resource)
  then
    moveToTrash resource
  else do
    forceDelete resource
    pure Nothing
  where
  msg :: String
  msg = "can not delete"

  moveToTrash :: R.Resource -> Aff _ (Maybe R.Resource)
  moveToTrash res = do
    let d = (res ^. R._root) </> dir Config.trashFolder
        path = (res # R._root .~ d) ^. R._path
    name <- getNewName d (res ^. R._name)
    move res (path # R._nameAnyPath .~ name)
    pure (Just $ R.Directory d)

  alreadyInTrash :: R.Resource -> Boolean
  alreadyInTrash res =
    case res ^. R._path of
      Left _ -> alreadyInTrash' (res ^. R._root)
      Right path -> alreadyInTrash' path

  alreadyInTrash' :: DirPath -> Boolean
  alreadyInTrash' d =
    if d == rootDir
    then false
    else maybe false go $ peel d

  go :: Tuple DirPath (Either DirName FileName) -> Boolean
  go (Tuple d name) =
    case name of
      Right _ -> false
      Left n -> if n == DirName Config.trashFolder then true else alreadyInTrash' d


move :: forall a e. R.Resource -> AnyPath -> Aff (RetryEffects (ajax :: AJAX | e)) AnyPath
move src tgt = do
  let url = if R.isDatabase src
            then Config.mountUrl
            else Config.dataUrl
  result <- slamjax $ defaultRequest
    { method = MOVE
    , headers = [RequestHeader "Destination" $ either printPath printPath tgt]
    , url = url <> R.resourcePath src
    }
  if succeeded result.status
     then pure tgt
     else throwError (error result.response)

mountInfo :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) String
mountInfo res = do
  result <- retryGet (Config.mountUrl <> R.resourcePath res)
  if succeeded result.status
     then case parse result.response of
       Left err -> throwError $ error (show err)
       Right uri -> pure uri
     else throwError (error result.response)
  where
  parse :: String -> F String
  parse = parseJSON >=> prop "mongodb" >=> readProp "connectionUri"

saveMount :: forall e. R.Resource -> String -> Aff (RetryEffects (ajax :: AJAX | e)) Unit
saveMount res uri = do
  result <- slamjax $ defaultRequest
    { method = PUT
    , headers = [ContentType applicationJSON]
    , content = Just $ stringify { mongodb: { connectionUri: uri } }
    , url = Config.mountUrl <> R.resourcePath res
    }
  if succeeded result.status
     then pure unit
     else throwError (error result.response)

foreign import stringify :: forall r. { | r } -> String

