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

module Quasar.Aff
  ( makeNotebook
  , loadNotebook
  , children
  , mountInfo
  , move
  , reqHeadersToJSON
  , saveMount
  , delete
  , getNewName
  , makeFile
  , ldJSON
  , getVersion

  , fields
  , templated
  , forceDelete
  , executeQuery
  , all
  , sample
  , transitiveChildrenProducer
  , query'
  , count

  , RetryEffects()
  ) where

import Prelude

import Config as Config
import Config.Paths as Config

import Control.Apply (lift2)
import Control.Bind ((>=>), (<=<))
import Control.Coroutine as CR
import Control.Coroutine.Aff as ACR
import Control.Monad (when)
import Control.Monad.Aff (Aff(), attempt, runAff)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Class (liftEff, MonadEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref (REF())
import Control.Monad.Error.Class (throwError)
import Control.MonadPlus (guard)
import Control.UI.Browser (encodeURIComponent)

import Data.Argonaut ((~>), (:=), (.?))
import Data.Argonaut as JS
import Data.Array as Arr
import Data.Bifunctor (bimap, lmap)
import Data.Date as Date
import Data.Either (Either(..), either)
import Data.Foldable (foldl, traverse_)
import Data.Foreign (Foreign(), F(), parseJSON)
import Data.Foreign.Class (readProp, read, IsForeign)
import Data.Foreign.Index (prop)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
import Data.Functor (($>))
import Data.Lens ((.~), (^.))
import Data.List as L
import Data.Maybe (Maybe(..), isJust, fromMaybe, maybe)
import Data.StrMap as SM
import Data.Path.Pathy as P
import Data.Path.Pathy ((</>))
import Data.String as S
import Data.Time (Milliseconds(..))
import Data.Tuple (Tuple(..))

import Model.Resource as R
import Model.Notebook as N

import Network.HTTP.Affjax
  ( Affjax(), AJAX(), AffjaxRequest(), AffjaxResponse(), RetryPolicy()
  , defaultRequest, affjax, retry, defaultRetryPolicy
  )
import Network.HTTP.Affjax.Request (Requestable)
import Network.HTTP.Affjax.Response (Respondable, ResponseType(JSONResponse))
import Network.HTTP.Method (Method(..))
import Network.HTTP.MimeType (MimeType(..), mimeTypeToString)
import Network.HTTP.MimeType.Common (applicationJSON)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))

import Unsafe.Coerce (unsafeCoerce)

import Utils.Path as PU
import Utils.Path ((<./>))


newtype Listing = Listing (Array R.Resource)

runListing :: Listing -> Array R.Resource
runListing (Listing rs) = rs

instance listingIsForeign :: IsForeign Listing where
  read f =
    readProp "children" f
      <#> runNullOrUndefined
      >>> fromMaybe []
      >>> Listing

instance listingRespondable :: Respondable Listing where
  responseType = JSONResponse
  fromResponse = read

children :: forall e. PU.DirPath -> Aff (RetryEffects (ajax :: AJAX | e)) (Array R.Resource)
children dir = do
  cs <- children' dir
  pure $ (R._root .~ dir) <$> cs

children' :: forall e. PU.DirPath -> Aff (RetryEffects (ajax :: AJAX | e)) (Array R.Resource)
children' dir = runListing <$> (getResponse msg $ listing dir)
  where
  msg = "Error: can not get children of resource"

listing :: forall e. PU.DirPath -> Affjax (RetryEffects e) Listing
listing p =
  case P.relativeTo p P.rootDir of
    Nothing -> throwError $ Exn.error "incorrect path"
    Just p ->
      getWithPolicy
        { shouldRetryWithStatusCode: \c -> not (succeeded c || c == notFoundStatus)
        , delayCurve: const 1000
        , timeout: Just 10000
        }
        (Config.metadataUrl </> p)

makeFile
  :: forall e
   . PU.FilePath
   -> MimeType
   -> String
   -> Aff (RetryEffects (ajax :: AJAX | e)) Unit
makeFile path mime content =
  getResponse msg go
  where
  msg :: String
  msg = "error while creating file"

  firstLine :: Maybe String
  firstLine = Arr.head $ S.split "\n" content

  isJson :: Either String JS.Json
  isJson = maybe (Left "empty file") Right firstLine >>= JS.jsonParser

  go :: Aff (RetryEffects (ajax :: AJAX | e)) (AffjaxResponse Unit)
  go = slamjax $ defaultRequest
   { method = PUT
   , headers = [ ContentType mime ]
   , content = Just content
   , url = fromMaybe "" $ (P.printPath <<< (Config.dataUrl </>)) <$> P.relativeTo path P.rootDir
   }


successStatus :: StatusCode
successStatus = StatusCode 200

notFoundStatus :: StatusCode
notFoundStatus = StatusCode 404

succeeded :: StatusCode -> Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = int

type RetryEffects e = (avar :: AVAR, ref :: REF, now :: Date.Now | e)

-- | A version of `affjax` with our retry policy.
slamjax :: forall e a b. (Requestable a, Respondable b) => AffjaxRequest a -> Affjax (RetryEffects e) b
slamjax = retry defaultRetryPolicy affjax

retryGet :: forall e a fd. (Respondable a) => P.Path P.Abs fd P.Sandboxed -> Affjax (RetryEffects e) a
retryGet =
  getWithPolicy
    { shouldRetryWithStatusCode: not <<< succeeded
    , delayCurve: const 1000
    , timeout: Just 30000
    }

getOnce :: forall e a fd. (Respondable a) => P.Path P.Abs fd P.Sandboxed -> Affjax (RetryEffects e) a
getOnce = getWithPolicy defaultRetryPolicy


getWithPolicy :: forall e a fd. (Respondable a) => RetryPolicy -> P.Path P.Abs fd P.Sandboxed -> Affjax (RetryEffects e) a
getWithPolicy policy u = do
  nocache <- liftEff $ Date.nowEpochMilliseconds
  retry policy affjax defaultRequest { url = url' nocache }
  where
  url' nocache = url ++ symbol ++ "nocache=" ++ pretty nocache
  url = P.printPath u
  symbol = if S.contains "?" url then "&" else "?"
  pretty (Milliseconds ms) =
    let s = show ms
    in fromMaybe s (S.stripSuffix ".0" s)

retryDelete :: forall e a fd. (Respondable a) => P.Path P.Abs fd P.Sandboxed -> Affjax (RetryEffects e) a
retryDelete u =
  slamjax $ defaultRequest { url = P.printPath u, method = DELETE }

retryPost :: forall e a b fd. (Requestable a, Respondable b) => P.Path P.Abs fd P.Sandboxed -> a -> Affjax (RetryEffects e) b
retryPost u c =
  slamjax $ defaultRequest { method = POST, url = P.printPath u, content = Just c }

retryPut :: forall e a b fd. (Requestable a, Respondable b) => P.Path P.Abs fd P.Sandboxed -> a -> MimeType -> Affjax (RetryEffects e) b
retryPut u c mime =
  slamjax $ defaultRequest
    { method = PUT
    , url = P.printPath u
    , content = Just c
    , headers = [ContentType mime]
    }

getResponse :: forall a e. String -> Affjax e a -> Aff (ajax :: AJAX | e) a
getResponse msg affjax = do
  res <- attempt affjax
  case res of
    Left e -> throwError $ Exn.error msg
    Right r -> do
      if not $ succeeded r.status
        then throwError $ Exn.error msg
        else pure r.response

reqHeadersToJSON :: Array RequestHeader -> JS.Json
reqHeadersToJSON = foldl go JS.jsonEmptyObject
  where
  go obj (Accept mime) = "Accept" := mimeTypeToString mime ~> obj
  go obj (ContentType mime) = "Content-Type" := mimeTypeToString mime ~> obj
  go obj (RequestHeader k v) = k := v ~> obj


mountInfo :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) String
mountInfo res = do
  result <- getOnce mountPath
  if succeeded result.status
     then case parse result.response of
       Left err ->
         throwError $ Exn.error (show err)
       Right uri ->
         pure uri
     else throwError (Exn.error result.response)

  where
  mountPath :: P.Path P.Abs P.Dir P.Sandboxed
  mountPath =
    (Config.mountUrl </> PU.rootify (R.resourceDir res)) #
      if R.resourceName res == ""
        then id
        else (</> (P.dir (R.resourceName res)))

  parse :: String -> F String
  parse = parseJSON >=> prop "mongodb" >=> readProp "connectionUri"


getNewName :: forall e. PU.DirPath -> String -> Aff (RetryEffects (ajax :: AJAX |e)) String
getNewName parent name = do
  items <- attempt (children' parent) <#> either (const []) id
  pure if exists' name items then getNewName' items 1 else name
  where
  getNewName' items i =
    let arr = S.split "." name
    in fromMaybe "" do
      body <- Arr.head arr
      suffixes <- Arr.tail arr
      let newName = S.joinWith "." $ Arr.cons (body <> " " <> show i) suffixes
      pure if exists' newName items
           then getNewName' items (i + one)
           else newName

exists' :: String -> Array R.Resource -> Boolean
exists' name items = isJust $ Arr.findIndex (\r -> r ^. R._name == name) items


-- Make dummy file in notebook specific folder.
-- It must be interpreted as empty notebook by notebook
-- component. Similar approach used in opening of data files, this
-- will help us decouple file and notebook subapps.
makeNotebook :: forall e. PU.DirPath -> Aff (RetryEffects (ajax :: AJAX |e)) String
makeNotebook path = do
  name <- getNewName path (Config.newNotebookName <> "." <> Config.notebookExtension)
  result <- retryPut (notebookPath name) N.emptyNotebook ldJSON
  if succeeded result.status
    then pure $ Config.notebookUrl
         <> "#" <> (PU.encodeURIPath $ P.printPath (path </> P.file name))
         <> "/edit"
    else throwError (Exn.error result.response)
  where
  notebookPath name =
    Config.dataUrl
      </> PU.rootify path
      </> P.dir name
      <./> Config.notebookExtension
      </> P.file "index"

move :: forall e. R.Resource -> PU.AnyPath -> Aff (RetryEffects (ajax :: AJAX |e)) PU.AnyPath
move src tgt = do
  let url = if R.isDatabase src || R.isViewMount src
            then Config.mountUrl
            else Config.dataUrl
  result <- slamjax $ defaultRequest
    { method = MOVE
    , headers = [RequestHeader "Destination" $ either P.printPath P.printPath tgt]
    , url =
        either
          (P.printPath <<< (url </>) <<< PU.rootifyFile)
          (P.printPath <<< (url </>) <<< PU.rootify)
          (R.getPath src)
    }
  if succeeded result.status
    then pure tgt
    else throwError (Exn.error result.response)

saveMount :: forall e. R.Resource -> String -> Aff (RetryEffects (ajax :: AJAX |e)) Unit
saveMount res uri = do
  result <- slamjax $ defaultRequest
    { method = PUT
    , headers = [ ContentType applicationJSON ]
    , content = Just $ stringify { mongodb: {connectionUri: uri } }
    , url = P.printPath $ Config.mountUrl </> PU.rootify (R.resourceDir res) </> P.dir (R.resourceName res)
    }
  if succeeded result.status
    then pure unit
    else throwError (Exn.error result.response)

foreign import stringify :: forall r. {|r} -> String

delete :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX |e)) (Maybe R.Resource)
delete resource =
  if not (R.isDatabase resource || alreadyInTrash resource || R.isViewMount resource)
  then
    moveToTrash resource
  else do
    forceDelete resource
    pure Nothing

  where
  msg :: String
  msg = "cannot delete"

  moveToTrash :: R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) (Maybe R.Resource)
  moveToTrash res = do
    let d = (res ^. R._root) </> P.dir Config.trashFolder
        path = (res # R._root .~ d) ^. R._path
    name <- getNewName d (res ^. R._name)
    move res (path # R._nameAnyPath .~ name)
    pure (Just $ R.Directory d)

  alreadyInTrash :: R.Resource -> Boolean
  alreadyInTrash res =
    case res ^. R._path of
      Left _ -> alreadyInTrash' (res ^. R._root)
      Right path -> alreadyInTrash' path

  alreadyInTrash' :: PU.DirPath -> Boolean
  alreadyInTrash' d =
    if d == P.rootDir
    then false
    else maybe false go $ P.peel d

  go :: Tuple PU.DirPath (Either P.DirName P.FileName) -> Boolean
  go (Tuple d name) =
    case name of
      Right _ -> false
      Left n ->
        if n == P.DirName Config.trashFolder
        then true
        else alreadyInTrash' d


forceDelete :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX |e)) Unit
forceDelete =
  getResponse "cannot delete"
  <<< either retryDelete retryDelete
  <<< pathFromResource

  where
  pathFromResource :: R.Resource -> PU.AnyPath
  pathFromResource r = transplant (rootForResource r) (R.getPath r)

  transplant :: PU.DirPath -> PU.AnyPath -> PU.AnyPath
  transplant newRoot =
    bimap
      (\p -> newRoot </> PU.rootifyFile p)
      (\p -> newRoot </> PU.rootify p)

  rootForResource :: R.Resource -> PU.DirPath
  rootForResource r =
    if R.isDatabase r || R.isViewMount r
    then Config.mountUrl
    else Config.dataUrl

getVersion :: forall e. Aff (RetryEffects (ajax :: AJAX |e)) (Maybe String)
getVersion = do
  serverInfo <- retryGet Config.Paths.serverInfoUrl
  return $ either (const Nothing) Just (readProp "version" serverInfo.response)

ldJSON :: MimeType
ldJSON = MimeType "application/ldjson"

loadNotebook :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX |e)) N.Notebook
loadNotebook res = do
  val <-
    getResponse "error loading notebook" $ retryGet $
      Config.dataUrl
        </> PU.rootify (R.resourceDir res)
        </> P.dir (R.resourceName res)
        </> P.file "index"
  case JS.decodeJson (foreignToJson val) of
    Left err -> throwError $ Exn.error err
    Right nb -> pure nb

  where
-- TODO: Not this. either add to Argonaut, or make a Respondable Json instance
-- (requires "argonaut core" - https://github.com/slamdata/purescript-affjax/issues/16#issuecomment-93565447)
  foreignToJson :: Foreign -> JS.Json
  foreignToJson = unsafeCoerce

-- | Produces a stream of the transitive children of a path
transitiveChildrenProducer
  :: forall e
   . PU.DirPath
  -> CR.Producer
      (Array R.Resource)
      (Aff (RetryEffects (ajax :: AJAX, err :: Exn.EXCEPTION | e)))
      Unit
transitiveChildrenProducer dirPath = do
  ACR.produce \emit -> do
    runAff Exn.throwException (const (pure unit)) $ do
      let
        go start = do
          ei <- attempt $ children start
          case ei of
            Right items -> do
              liftEff $ emit (Left items)
              let parents = Arr.mapMaybe (either (const Nothing) Just <<< R.getPath) items
              traverse_ go parents
            Left _ ->
              liftEff $ emit (Right unit)
      go dirPath



-- | This is template string where actual path is encoded like {{path}}
type SQL = String

query :: forall e. R.Resource -> SQL -> Aff (RetryEffects (ajax :: AJAX | e)) JS.JArray
query res sql =
  if not $ R.isFile res
  then pure []
  else extractJArray <$> (getResponse msg $ retryGet uriPath)
  where
  msg = "error in query"
  uriPath = mkURI res sql

query' :: forall e. R.Resource -> SQL -> Aff (RetryEffects (ajax :: AJAX | e)) (Either String JS.JArray)
query' res@(R.File _) sql = do
  result <- retryGet (mkURI' res sql)
  pure if succeeded result.status
       then Right (extractJArray result.response)
       else Left $ readError "error in query" result.response

query' _ _ = pure $ Left "Query resource is not a file"

count :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) Int
count res = do
  fromMaybe 0 <<< readTotal <$> query res sql
  where
  sql :: SQL
  sql = "SELECT COUNT(*) as total FROM {{path}}"

  readTotal :: JS.JArray -> Maybe Int
  readTotal =
    Data.Int.fromNumber
      <=< JS.toNumber
      <=< SM.lookup "total"
      <=< JS.toObject
      <=< Arr.head


portView
  :: forall e
   . R.Resource
  -> R.Resource
  -> SQL
  -> SM.StrMap String
  -> Aff (RetryEffects (ajax :: AJAX | e)) Unit
portView res dest sql varMap = do
  guard $ R.isFile dest
  let
    queryParams = maybe "" ("&" <>) $ renderQueryString varMap
    connectionUri = "sql2:///?q=" <> PU.encodeURIPath (templated res sql) <> queryParams
  result <-
    slamjax $ defaultRequest
      { method = PUT
      , headers = [ ContentType applicationJSON ]
      , content = Just $ stringify { view: { connectionUri: connectionUri } }
      , url =
          P.printPath $
            Config.mountUrl
              </> PU.rootify (R.resourceDir dest)
              </> P.file (R.resourceName dest)
      }
  if succeeded result.status
     then pure unit
     else throwError $ Exn.error $ readError result.response result.response

portQuery
  :: forall e
   . R.Resource
  -> R.Resource
  -> SQL
  -> SM.StrMap String
  -> Aff (RetryEffects (ajax :: AJAX | e)) JS.JObject
portQuery res dest sql vars = do
  guard $ R.isFile dest
  result <-
    slamjax $ defaultRequest
      { method = POST
      , headers =
          [ RequestHeader "Destination" $ R.resourcePath dest
          , ContentType ldJSON
          ]
      , url =
          P.printPath $
            Config.queryUrl
              </> PU.rootify (R.resourceDir res)
              </> P.dir (R.resourceName res)
              </> P.file queryVars
      , content = Just (templated res sql)
      }

  if not $ succeeded result.status
    then throwError $ Exn.error $ readError result.response result.response
    else
    -- We expect result message to be valid json.
    either (throwError <<< Exn.error) pure $
      JS.jsonParser result.response >>= JS.decodeJson
  where
  queryVars :: String
  queryVars = maybe "" ("?" <>) $ renderQueryString vars

renderQueryString :: SM.StrMap String -> Maybe String
renderQueryString = map go <<< L.uncons <<< SM.toList
  where
  pair :: Tuple String String -> String
  pair (Tuple a b) = "var." <> a <> "=" <> encodeURIComponent b

  go { head = h, tail = t } =
    foldl (\a v -> a <> "&" <> pair v) (pair h) t


readError :: String -> String -> String
readError msg input =
  let responseError = JS.jsonParser input >>= JS.decodeJson >>= (.? "error")
  in either (const msg) id responseError

sample' :: forall e. R.Resource -> Maybe Int -> Maybe Int -> Aff (RetryEffects (ajax :: AJAX | e)) JS.JArray
sample' res mbOffset mbLimit =
  if not $ R.isFile res
  then pure []
  else extractJArray <$> (getResponse msg $ retryGet uri)
  where
  msg = "error getting resource sample"
  uri =
    Config.dataUrl
      </> PU.rootify (R.resourceDir res)
      </> P.file
            (R.resourceName res
               <> (maybe "" (("?offset=" <>) <<< show) mbOffset)
               <> (maybe "" (("&limit=" <>) <<< show ) mbLimit))


sample :: forall e. R.Resource -> Int -> Int -> Aff (RetryEffects (ajax :: AJAX | e)) JS.JArray
sample res offset limit = sample' res (Just offset) (Just limit)

all :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) JS.JArray
all res = sample' res Nothing Nothing

fields :: forall e. R.Resource -> Aff (RetryEffects (ajax :: AJAX | e)) (Array String)
fields res = do
  jarr <- sample res 0 100
  case jarr of
    [] -> throwError $ Exn.error "empty file"
    _ -> pure $ Arr.nub $ Arr.concat (getFields <$> jarr)

mkURI :: R.Resource -> SQL -> PU.FilePath
mkURI res sql = Config.queryUrl </> P.file ("?q=" <> encodeURIComponent (templated res sql))

mkURI' :: R.Resource -> SQL -> PU.FilePath
mkURI' res sql =
  Config.queryUrl
  </> PU.rootify (R.resourceDir res)
  </> P.dir (R.resourceName res)
  </> P.file ("?q=" <> encodeURIComponent (templated res sql))


templated :: R.Resource -> SQL -> SQL
templated res = S.replace "{{path}}" ("\"" <> R.resourcePath res <> "\"")

extractJArray :: String -> JS.JArray
extractJArray =
  foldl folder [] <<< map JS.jsonParser <<< S.split "\n"
  where
  folder :: JS.JArray -> Either String JS.Json -> JS.JArray
  folder agg (Right j) = agg ++ [j]
  folder agg _ = agg

getFields :: JS.Json -> Array String
getFields json = Arr.filter (/= "") $ Arr.nub $ getFields' [] json

getFields' :: Array String -> JS.Json -> Array String
getFields' [] json = getFields' [""] json
getFields' acc json =
  if JS.isObject json
  then maybe acc (goObj acc) $ JS.toObject json
  else if JS.isArray json
       then maybe acc (goArr acc) $ JS.toArray json
       else acc

  where
  goArr :: Array String -> JS.JArray -> Array String
  goArr acc arr =
    Arr.concat $ getFields' (lift2 append acc $ mkArrIxs arr) <$> arr
    where
    mkArrIxs :: JS.JArray -> Array String
    mkArrIxs jarr =
      map (show >>> \x -> "[" <> x <> "]") $ Arr.range 0 $ Arr.length jarr - 1

  goObj :: Array String -> JS.JObject -> Array String
  goObj acc = Arr.concat <<< map (goTuple acc) <<< L.fromList <<< SM.toList

  goTuple :: Array String -> Tuple String JS.Json -> Array String
  goTuple acc (Tuple key json) =
    getFields' ((\x -> x <> ".\"" <> key <> "\"") <$> acc) json

executeQuery
  :: forall e
   . String
  -> Boolean
  -> SM.StrMap String
  -> R.Resource
  -> R.Resource
  -> Aff (RetryEffects (ajax :: AJAX | e)) (Either String { outputResource :: R.Resource, plan :: Maybe String })
executeQuery sql cachingEnabled varMap inputResource outputResource = do
  when (R.isTempFile outputResource) $
    void $ attempt $ forceDelete outputResource

  ejobj <- do
    attempt $
      if cachingEnabled
         then portQuery inputResource outputResource sql varMap <#> Just
         else portView inputResource outputResource sql varMap $> Nothing

  pure $ do
    mjobj <- lmap Exn.message ejobj
    info <-
      case mjobj of
        Nothing -> do
          path <- R.getPath outputResource # either pure \_ -> Left "Expected output resource as file or view mount"
          sandboxedPath <- P.sandbox P.rootDir path # maybe (Left "Could not sandbox output resource") pure
          pure
            { sandboxedPath
            , plan: Nothing
            }
        Just jobj -> do
          planPhases <- Arr.last <$> jobj .? "phases"
          sandboxedPath <- do
            pathString <- jobj .? "out"
            path <- P.parseAbsFile pathString # maybe (Left "Invalid file from Quasar") pure
            P.sandbox P.rootDir path # maybe (Left "Could not sandbox Quasar file") pure
          pure
            { sandboxedPath
            , plan: planPhases >>= (.? "detail") >>> either (const Nothing) Just
            }
    pure
      { outputResource: R.mkFile $ Left $ P.rootDir </> info.sandboxedPath
      , plan: info.plan
      }
