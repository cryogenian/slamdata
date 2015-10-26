{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Quasar.Aff where

import Prelude

import Config as Config
import Config.Paths as Config
import Control.Apply ((*>))
import Control.Bind ((>=>))
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF())
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Combinators ((~>), (:=))
import Data.Argonaut.Core (Json(), JAssoc(), jsonEmptyObject)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head, tail, (:), findIndex)
import Data.Bifunctor (bimap)
import Data.Date (nowEpochMilliseconds, now, Now())
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Foreign (F(), Foreign(), parseJSON)
import Data.Foreign.Class (readProp, read, IsForeign)
import Data.Foreign.Index (prop)
import Data.Maybe (Maybe(..), isJust, fromMaybe, maybe)
import Data.Path.Pathy
  (Path(),
   Abs(),
   Sandboxed(),
   rootDir,
   relativeTo,
   (</>),
   printPath,
   dir,
   file,
   peel,
   DirName(..),
   FileName(..))
import Utils.Path
  (DirPath(),
   FilePath(),
   AnyPath(),
   rootify,
   rootifyFile,
   (<./>),
   dropNotebookExt,
   encodeURIPath)
import Model.Resource as R
import Data.String as S
import Data.These (These(..))
import Data.Time (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax
  (Affjax(), AJAX(), URL(), AffjaxRequest(), RetryPolicy(), defaultRequest, affjax, retry, defaultRetryPolicy)
import Network.HTTP.Affjax (Affjax(), AJAX(), affjax, defaultRequest)
import Network.HTTP.Affjax.Request (Requestable)
import Network.HTTP.Affjax.Response (Respondable, ResponseType(JSONResponse))
import Network.HTTP.Method (Method(..))
import Network.HTTP.MimeType (MimeType(..), mimeTypeToString)
import Network.HTTP.MimeType.Common (applicationJSON)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Optic.Core ((.~), (^.))

newtype Listing = Listing (Array R.Resource)

runListing :: Listing -> Array R.Resource
runListing (Listing rs) = rs

instance listingIsForeign :: IsForeign Listing where
  read f = Listing <$> readProp "children" f

instance listingRespondable :: Respondable Listing where
  responseType = JSONResponse
  fromResponse = read

children :: forall e.
            DirPath -> Aff (RetryEffects (ajax :: AJAX | e)) (Array R.Resource)
children dir = do
  cs <- children' dir
  pure $ (R._root .~ dir) <$> cs

children' :: forall e.
             DirPath -> Aff (RetryEffects (ajax :: AJAX | e)) (Array R.Resource)
children' dir = runListing <$> (getResponse msg $ listing dir)
  where
  msg = "Error: can not get children of resource"

listing :: forall e. DirPath -> Affjax (RetryEffects e) Listing
listing p = maybe (throwError $ error "incorrect path")
            (\p -> retryGet $ (Config.metadataUrl </> p))
            $ relativeTo p rootDir



makeFile :: forall e. FilePath -> MimeType -> String
            -> Aff (RetryEffects (ajax :: AJAX | e)) Unit
makeFile path mime content =
  getResponse msg go
  where
  msg :: String
  msg = "error while creating file"

  err :: _ -> Aff _ _
  err _ = throwError $ error "file has incorrect format"

  firstLine :: Maybe String
  firstLine = head $ S.split "\n" content

  isJson :: Either _ _
  isJson = maybe (Left "empty file") Right firstLine >>= jsonParser

  go :: Aff _ _
  go = slamjax $ defaultRequest
   { method = PUT
   , headers = [ ContentType mime ]
   , content = Just content
   , url = fromMaybe ""
           $ (\x -> printPath
                    $ Config.dataUrl
                    </> x)
           <$> (relativeTo path rootDir)
   }


successStatus :: StatusCode
successStatus = StatusCode 200

succeeded :: StatusCode -> Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = int

type RetryEffects e = (avar :: AVAR, ref :: REF, now :: Now | e)

-- | A version of `affjax` with our retry policy.
slamjax :: forall e a b. (Requestable a, Respondable b) =>
           AffjaxRequest a -> Affjax (RetryEffects e) b
slamjax = retry defaultRetryPolicy affjax

retryGet :: forall e a fd. (Respondable a) =>
            Path Abs fd Sandboxed -> Affjax (RetryEffects e) a
retryGet =
  getWithPolicy { shouldRetryWithStatusCode: not <<< succeeded
                , delayCurve: const 1000
                , timeout: Just 30000
                }
getOnce :: forall e a fd. (Respondable a) =>
           Path Abs fd Sandboxed -> Affjax (RetryEffects e) a
getOnce = getWithPolicy defaultRetryPolicy


getWithPolicy :: forall e a fd. (Respondable a) =>
                 RetryPolicy -> Path Abs fd Sandboxed
                 -> Affjax (RetryEffects e) a
getWithPolicy policy u = do
  nocache <- liftEff $ nowEpochMilliseconds
  retry policy affjax defaultRequest { url = url' nocache }
  where
  url' nocache = url ++ symbol ++ "nocache=" ++ pretty nocache
  url = printPath u
  symbol = if S.contains "?" url then "&" else "?"
  pretty (Milliseconds ms) =
    let s = show ms
    in fromMaybe s (S.stripSuffix ".0" s)

retryDelete :: forall e a fd. (Respondable a) =>
               Path Abs fd Sandboxed -> Affjax (RetryEffects e) a
retryDelete u =
  slamjax $ defaultRequest { url = printPath u, method = DELETE }

retryPost :: forall e a b fd. (Requestable a, Respondable b) =>
             Path Abs fd Sandboxed -> a -> Affjax (RetryEffects e) b
retryPost u c =
  slamjax $ defaultRequest { method = POST, url = printPath u, content = Just c }

retryPut :: forall e a b fd. (Requestable a, Respondable b)
            => Path Abs fd Sandboxed -> a -> MimeType -> Affjax (RetryEffects e) b
retryPut u c mime =
  slamjax $ defaultRequest { method = PUT
                           , url = printPath u
                           , content = Just c
                           , headers = [ContentType mime] }

getResponse :: forall a e. String -> Affjax e a -> Aff (ajax :: AJAX | e) a
getResponse msg affjax = do
  res <- attempt affjax
  case res of
    Left e -> throwError $ error msg
    Right r -> do
      if not $ succeeded r.status
        then throwError $ error msg
        else pure r.response

reqHeadersToJSON :: Array RequestHeader -> Json
reqHeadersToJSON = foldl go jsonEmptyObject
  where
  go obj (Accept mime) = "Accept" := mimeTypeToString mime ~> obj
  go obj (ContentType mime) = "Content-Type" := mimeTypeToString mime ~> obj
  go obj (RequestHeader k v) = k := v ~> obj


mountInfo :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) String
mountInfo res = do

  let mountPath = (Config.mountUrl </> rootify (R.resourceDir res)) #
                  if R.resourceName res == ""
                  then id
                  else \x -> x </> dir (R.resourceName res)
  result <- getOnce mountPath
  if succeeded result.status
     then case parse result.response of
       Left err ->
         throwError $ error (show err)
       Right uri ->
         pure uri
     else throwError (error result.response)

  where
  parse :: String -> F String
  parse = parseJSON >=> prop "mongodb" >=> readProp "connectionUri"


getNewName :: forall e. DirPath -> String -> Aff (RetryEffects (ajax :: AJAX |e)) String
getNewName parent name = do
  items <- children' parent
  pure if exists' name items then getNewName' items 1 else name
  where
  getNewName' items i =
    let arr = S.split "." name
    in fromMaybe "" do
      body <- head arr
      suffixes <- tail arr
      let newName = S.joinWith "." $ (body <> " " <> show i):suffixes
      pure if exists' newName items
           then getNewName' items (i + one)
           else newName

exists' :: String -> Array R.Resource -> Boolean
exists' name items = isJust $ findIndex (\r -> r ^. R._name == name) items


-- Make dummy file in notebook specific folder.
-- It must be interpreted as empty notebook by notebook
-- component. Similar approach used in opening of data files, this
-- will help us decouple file and notebook subapps.
makeNotebook :: forall e. DirPath -> Aff (RetryEffects (ajax :: AJAX |e)) String
makeNotebook path = do
  name <- getNewName path (Config.newNotebookName <> "." <> Config.notebookExtension)
  result <- retryPut (notebookPath name) notebookDummy ldJSON
  if succeeded result.status
    then pure $ Config.notebookUrl
         <> "#" <> (encodeURIPath $ printPath (path </> file name))
         <> "/edit"
    else throwError (error result.response)
  where
  notebookPath name = Config.dataUrl
                      </> rootify path
                      </> dir name
                      <./> Config.notebookExtension
                      </> file "index"
  notebookDummy = """{"type": "new"}"""

move :: forall a e. R.Resource -> AnyPath -> Aff (RetryEffects (ajax :: AJAX |e)) AnyPath
move src tgt = do
  let url = if R.isDatabase src
            then Config.mountUrl
            else Config.dataUrl
  result <- slamjax $ defaultRequest
    { method = MOVE
    , headers = [RequestHeader "Destination" $ either printPath printPath tgt]
    , url = either
            (printPath <<< (url </>) <<< rootifyFile)
            (printPath <<< (url </>) <<< rootify)
            $ R.getPath src
    }
  if succeeded result.status
    then pure tgt
    else throwError (error result.response)

saveMount :: forall e. R.Resource -> String
             -> Aff (RetryEffects (ajax :: AJAX |e)) Unit
saveMount res uri = do
  result <- slamjax $ defaultRequest
            { method = PUT
            , headers = [ ContentType applicationJSON ]
            , content = Just $ stringify { mongodb: {connectionUri: uri } }
            , url = printPath
                    $ Config.mountUrl
                    </> rootify (R.resourceDir res)
                    </> dir (R.resourceName res)
            }
  if succeeded result.status
    then pure unit
    else throwError (error result.response)

foreign import stringify :: forall r. {|r} -> String

delete :: forall e. R.Resource
          -> Aff (RetryEffects (ajax :: AJAX |e)) (Maybe R.Resource)
delete resource =
  if not (R.isDatabase resource || alreadyInTrash resource)
  then
    moveToTrash resource
  else do
    forceDelete resource
    pure Nothing
  where
  msg :: String
  msg = "cannot delete"

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
      Left n -> if n == DirName Config.trashFolder
                then true
                else alreadyInTrash' d


forceDelete :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX |e)) Unit
forceDelete =
  getResponse "cannot delete"
  <<< either retryDelete retryDelete
  <<< pathFromResource

  where
  pathFromResource :: R.Resource -> AnyPath
  pathFromResource r = transplant (rootForResource r) (R.getPath r)

  transplant :: DirPath -> AnyPath -> AnyPath
  transplant newRoot =
    bimap
    (\p -> newRoot </> rootifyFile p)
    (\p -> newRoot </> rootify p)

  rootForResource :: R.Resource -> DirPath
  rootForResource r =
    if R.isDatabase r
    then Config.mountUrl
    else Config.dataUrl

getVersion :: forall e. Aff (RetryEffects (ajax :: AJAX |e)) (Maybe String)
getVersion = do
  serverInfo <- retryGet Config.Paths.serverInfoUrl
  return $ either (const Nothing) Just (readProp "version" serverInfo.response)

ldJSON :: MimeType
ldJSON = MimeType "application/ldjson"
