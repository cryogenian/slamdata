{-
Copyright 2016 SlamData, Inc.

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
  ( children
  , mountInfo
  , viewInfo
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

  , save
  , load
  , resourceExists
  , portView
  , RetryEffects()
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Bind ((>=>), (<=<), (=<<))
import Control.Coroutine as CR
import Control.Coroutine.Aff as ACR
import Control.Monad (when, unless)
import Control.Monad.Aff (Aff())
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Error.Class as Err
import Control.MonadPlus (guard)
import Control.UI.Browser (encodeURIComponent)

import Data.Argonaut ((~>), (:=), (.?))
import Data.Argonaut as JS
import Data.Array as Arr
import Data.Bifunctor (bimap, lmap)
import Data.Date as Date
import Data.Either as E
import Data.Foldable as F

import Data.Foreign (F(), parseJSON)
import Data.Foreign.Class (readProp, read, IsForeign)
import Data.Foreign.Index (prop)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)

import Data.Functor (($>))
import Data.Functor.Eff (liftEff)
import Data.Lens ((.~), (^.))
import Data.List as L
import Data.Maybe as M
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as Set
import Data.String as S
import Data.StrMap as SM
import Data.Time as Time
import Data.Tuple (Tuple(..))
import Data.URI as URI
import Data.URI.Types as URI

import DOM (DOM())

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request as AX
import Network.HTTP.Affjax.Response as AX
import Network.HTTP.Method (Method(..))
import Network.HTTP.MimeType (MimeType(..), mimeTypeToString)
import Network.HTTP.MimeType.Common (applicationJSON)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))

import Quasar.Paths as Paths
import Quasar.Auth as Auth

-- TODO: split out a core Quasar module that only deals with the API, and
-- doesn't know about SlamData specific things.
import SlamData.Config as Config
import SlamData.FileSystem.Resource as R

import Utils.Completions (memoizeCompletionStrs)
import Utils.Path as PU

newtype Listing = Listing (Array R.Resource)

runListing :: Listing -> Array R.Resource
runListing (Listing rs) = rs

instance listingIsForeign :: IsForeign Listing where
  read f =
    read f
      >>= readProp "children"
      >>= pure
        <<< Listing
        <<< M.fromMaybe []
        <<< runNullOrUndefined

instance listingRespondable :: AX.Respondable Listing where
  responseType = Tuple (M.Just applicationJSON) AX.JSONResponse
  fromResponse = read

children
  :: forall e
   . PU.DirPath
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX, dom :: DOM | e)) (Array R.Resource)
children dir idToken = do
  cs <- children' dir idToken
  let result = (R._root .~ dir) <$> cs
  memoizeCompletionStrs dir result
  pure result

children'
  :: forall e
   . PU.DirPath
   -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) (Array R.Resource)
children' dir idToken =
  listing dir idToken
    # getResponse msg
    <#> runListing
  where
  msg = "Error: can not get children of resource"

listing
  :: forall e
   . PU.DirPath
  -> M.Maybe Auth.IdToken
  -> AX.Affjax (RetryEffects e) Listing
listing p idToken =
  case P.relativeTo p P.rootDir of
    M.Nothing -> Err.throwError $ Exn.error "incorrect path"
    M.Just p ->
      retryGet
        (Paths.metadataUrl </> p)
        applicationJSON
        idToken

makeFile
  :: forall e
   . PU.FilePath
  -> MimeType
  -> String
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) Unit
makeFile path mime content idToken =
  getResponse "error while creating file" $
    slamjax $ AX.defaultRequest
      { method = PUT
      , headers = [ ContentType mime ] <> M.maybe [] (\x -> [Auth.authHeader x]) idToken
      , content = M.Just content
      , url = M.fromMaybe "" $ (P.printPath <<< (Paths.dataUrl </>)) <$> P.relativeTo path P.rootDir
      }


successStatus :: StatusCode
successStatus = StatusCode 200

notFoundStatus :: StatusCode
notFoundStatus = StatusCode 404

succeeded :: StatusCode -> Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = int

type RetryEffects e = (avar :: AVar.AVAR, ref :: Ref.REF, now :: Date.Now | e)

-- | A version of `affjax` with our retry policy.
slamjax
  :: forall e a b
   . (AX.Requestable a, AX.Respondable b)
  => AX.AffjaxRequest a
  -> AX.Affjax (RetryEffects e) b
slamjax =
  AX.retry
    AX.defaultRetryPolicy
    AX.affjax

retryGet
  :: forall e a fd
   . (AX.Respondable a)
  => P.Path P.Abs fd P.Sandboxed
  -> MimeType
  -> M.Maybe Auth.IdToken
  -> AX.Affjax (RetryEffects e) a
retryGet =
  getWithPolicy $
    AX.defaultRetryPolicy
      { delayCurve = const 1000
      , timeout = M.Just 30000
      }

mkRequest
  :: forall e fd
   . P.Path P.Abs fd P.Sandboxed
  -> MimeType
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects e) (AX.AffjaxRequest Unit)
mkRequest u mime idToken = do
  nocache <- liftEff $ Date.nowEpochMilliseconds
  pure $
    AX.defaultRequest
      { url = url' nocache
      , headers = [ Accept mime ] <> M.maybe [] (\t -> [ Auth.authHeader t ]) idToken
      }
  where
  url' nocache = url <> symbol <> "nocache=" <> pretty nocache
  symbol = if S.contains "?" url then "&" else "?"
  pretty (Time.Milliseconds ms) = let s = show ms in M.fromMaybe s (S.stripSuffix ".0" s)
  url = P.printPath u

getOnce
  :: forall e a fd
   . (AX.Respondable a)
  => P.Path P.Abs fd P.Sandboxed
  -> MimeType
  -> M.Maybe Auth.IdToken
  -> AX.Affjax (RetryEffects e) a
getOnce u mime idToken =
  mkRequest u mime idToken
    >>= AX.affjax

getWithPolicy
  :: forall e a fd
   . (AX.Respondable a)
  => AX.RetryPolicy
  -> P.Path P.Abs fd P.Sandboxed
  -> MimeType
  -> M.Maybe Auth.IdToken
  -> AX.Affjax (RetryEffects e) a
getWithPolicy policy u mime idToken =
  mkRequest u mime idToken
    >>= AX.retry policy AX.affjax

retryDelete
  :: forall e a fd
   . (AX.Respondable a)
  => P.Path P.Abs fd P.Sandboxed
  -> M.Maybe Auth.IdToken
  -> AX.Affjax (RetryEffects e) a
retryDelete u idToken = do
  slamjax $ AX.defaultRequest
    { url = P.printPath u
    , method = DELETE
    , headers = M.maybe [] (\t -> [Auth.authHeader t]) idToken
    }

retryPost
  :: forall e a b fd
   . (AX.Requestable a, AX.Respondable b)
  => P.Path P.Abs fd P.Sandboxed
  -> a
  -> M.Maybe Auth.IdToken
  -> AX.Affjax (RetryEffects e) b
retryPost u c idToken =
  slamjax $ AX.defaultRequest
    { method = POST
    , url = P.printPath u
    , content = M.Just c
    , headers = M.maybe [] (\t -> [Auth.authHeader t]) idToken
    }

retryPut
  :: forall e a b fd
   . (AX.Requestable a, AX.Respondable b)
  => P.Path P.Abs fd P.Sandboxed
  -> a
  -> MimeType
  -> M.Maybe Auth.IdToken
  -> AX.Affjax (RetryEffects e) b
retryPut u c mime idToken =
  slamjax $ AX.defaultRequest
    { method = PUT
    , url = P.printPath u
    , content = M.Just c
    , headers = [ContentType mime] <> M.maybe [] (\t -> [Auth.authHeader t]) idToken
    }

getResponse :: forall a e. String -> AX.Affjax e a -> Aff (ajax :: AX.AJAX | e) a
getResponse msg m = do
  res <- Aff.attempt m
  case res of
    E.Left e -> Err.throwError $ Exn.error msg
    E.Right r -> do
      if not $ succeeded r.status
        then Err.throwError $ Exn.error msg
        else pure r.response

reqHeadersToJSON :: Array RequestHeader -> JS.Json
reqHeadersToJSON = F.foldl go JS.jsonEmptyObject
  where
  go obj (Accept mime) = "Accept" := mimeTypeToString mime ~> obj
  go obj (ContentType mime) = "Content-Type" := mimeTypeToString mime ~> obj
  go obj (RequestHeader k v) = k := v ~> obj


mountInfo
  :: forall e
   . R.Resource
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) String
mountInfo res idToken = do
  result <- getOnce mountPath applicationJSON idToken
  if succeeded result.status
     then case parse result.response of
       E.Left err ->
         Err.throwError $ Exn.error (show err)
       E.Right uri ->
         pure uri
     else Err.throwError (Exn.error result.response)

  where
  mountPath :: P.Path P.Abs P.Dir P.Sandboxed
  mountPath =
    (Paths.mountUrl </> PU.rootify (R.resourceDir res)) #
      if R.resourceName res == ""
        then id
        else (</> (P.dir (R.resourceName res)))

  parse :: String -> F String
  parse = parseJSON >=> prop "mongodb" >=> readProp "connectionUri"

viewInfo
  :: forall e
   . R.Resource
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX|e)) { sql :: String, vars :: SM.StrMap String }
viewInfo (R.ViewMount mountPath) idToken = do
  result <- getOnce (Paths.mountUrl </> PU.rootifyFile mountPath) applicationJSON idToken
  if succeeded result.status
    then case parse result.response of
      E.Left err -> Err.throwError $ Exn.error err
      E.Right res -> pure res
    else Err.throwError $ Exn.error result.response
  where
  runQuery :: URI.Query -> SM.StrMap (M.Maybe String)
  runQuery (URI.Query q) = q

  parse :: String -> E.Either String { sql :: String, vars :: SM.StrMap String }
  parse connURI = do
    connStr <-
      lmap show
      $ parseJSON connURI
      >>= prop "view"
      >>= readProp "connectionUri"
    URI.AbsoluteURI mbScheme _ mbQuery <- lmap show $ URI.runParseAbsoluteURI connStr
    scheme <- M.maybe (Err.throwError "There is no scheme") pure mbScheme
    unless (scheme == URI.URIScheme "sql2") $ Err.throwError "Incorrect scheme"

    let queryMap = M.maybe SM.empty runQuery mbQuery
    sql <-
      M.maybe (Err.throwError "There is no 'q' in queryMap") pure
        $ SM.lookup "q" queryMap
        >>= id
        >>> map PU.decodeURIPath
        >>= S.stripPrefix "("
        >>= S.stripSuffix ")"
    let vars = SM.fold foldFn SM.empty $ SM.delete "q" queryMap
    pure { vars, sql }
    where
    foldFn :: SM.StrMap String -> String -> M.Maybe String -> SM.StrMap String
    foldFn acc key mbVal = M.fromMaybe acc do
      k <- S.stripPrefix "var." key
      val <- mbVal
      pure $ SM.insert k val acc
viewInfo _ _ = Err.throwError $ Exn.error "Incorrect resource in viewInfo"



-- | Generates a new resource name based on a directory path and a name for the
-- | resource. If the name already exists in the path a number is appended to
-- | the end of the name.
getNewName
  :: forall e
   . PU.DirPath
  -> String
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX |e)) String
getNewName parent name idToken = do
  items <- Aff.attempt (children' parent idToken) <#> E.either (const []) id
  pure if exists' name items then getNewName' items 1 else name
  where
  getNewName' items i =
    let arr = S.split "." name
    in M.fromMaybe "" do
      body <- Arr.head arr
      suffixes <- Arr.tail arr
      let newName = S.joinWith "." $ Arr.cons (body <> " " <> show i) suffixes
      pure if exists' newName items
           then getNewName' items (i + one)
           else newName

exists' :: String -> Array R.Resource -> Boolean
exists' name items = M.isJust $ Arr.findIndex (\r -> r ^. R._name == name) items

-- | Will return `Just` in case the resource was successfully moved, and
-- | `Nothing` in case no resource existed at the requested source path.
move
  :: forall e
   . R.Resource
  -> PU.AnyPath
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX, dom :: DOM |e)) (M.Maybe PU.AnyPath)
move src tgt idToken = do
  let url = if R.isDatabase src || R.isViewMount src
            then Paths.mountUrl
            else Paths.dataUrl
  cleanViewMounts src idToken
  result <- AX.affjax $ AX.defaultRequest
    { method = MOVE
    , headers = [RequestHeader "Destination" $ E.either P.printPath P.printPath tgt] <> M.maybe [] (\t -> [Auth.authHeader t]) idToken
    , url =
        E.either
          (P.printPath <<< (url </>) <<< PU.rootifyFile)
          (P.printPath <<< (url </>) <<< PU.rootify)
          (R.getPath src)
    }
  if succeeded result.status
    then pure $ M.Just tgt
    else if result.status == notFoundStatus
      then pure M.Nothing
      else Err.throwError (Exn.error result.response)

saveMount
  :: forall e
   . R.Resource
  -> String
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX |e)) Unit
saveMount res uri idToken = do
  result <- slamjax $ AX.defaultRequest
    { method = PUT
    , headers = [ ContentType applicationJSON ] <> M.maybe [] (\t -> [ Auth.authHeader t ]) idToken
    , content = M.Just $ stringify { mongodb: {connectionUri: uri } }
    , url = P.printPath
            $ Paths.mountUrl
            </> PU.rootify (R.resourceDir res)
            </> P.dir (R.resourceName res)
    }
  if succeeded result.status
    then pure unit
    else Err.throwError (Exn.error result.response)

foreign import stringify :: forall r. {|r} -> String

delete
  :: forall e
   . R.Resource
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX, dom :: DOM |e)) (M.Maybe R.Resource)
delete resource idToken =
  if not (R.isDatabase resource || alreadyInTrash resource || R.isViewMount resource)
  then (moveToTrash resource) <|> (forceDelete resource idToken $> M.Nothing)
  else forceDelete resource idToken $> M.Nothing

  where
  msg :: String
  msg = "cannot delete"

  moveToTrash
    :: R.Resource
    -> Aff (RetryEffects (ajax :: AX.AJAX, dom :: DOM | e)) (M.Maybe R.Resource)
  moveToTrash res = do
    let d = (res ^. R._root) </> P.dir Config.trashFolder
        path = (res # R._root .~ d) ^. R._path
    name <- getNewName d (res ^. R._name) idToken
    move res (path # R._nameAnyPath .~ name) idToken
    pure $ M.Just $ R.Directory d

  alreadyInTrash :: R.Resource -> Boolean
  alreadyInTrash res =
    case res ^. R._path of
      E.Left _ -> alreadyInTrash' (res ^. R._root)
      E.Right path -> alreadyInTrash' path

  alreadyInTrash' :: PU.DirPath -> Boolean
  alreadyInTrash' d =
    if d == P.rootDir
    then false
    else M.maybe false go $ P.peel d

    where
    go :: Tuple PU.DirPath (E.Either P.DirName P.FileName) -> Boolean
    go (Tuple d name) =
      case name of
        E.Right _ -> false
        E.Left n ->
          if n == P.DirName Config.trashFolder
          then true
          else alreadyInTrash' d


forceDelete
  :: forall e
   . R.Resource
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX, dom :: DOM |e)) Unit
forceDelete res idToken = do
  cleanViewMounts res idToken
  getResponse "cannot delete"
    $ flip (E.either retryDelete retryDelete) idToken
    $ pathFromResource res

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
    then Paths.mountUrl
    else Paths.dataUrl

cleanViewMounts
  :: forall e
   . R.Resource
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX, dom :: DOM|e)) Unit
cleanViewMounts res idToken =
  F.for_ (R.getPath res) \dirPath ->
    children dirPath idToken
      >>= Arr.filter R.isViewMount
      >>> F.traverse_ (flip forceDelete idToken)

getVersion
  :: forall e
   . M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX |e)) (M.Maybe String)
getVersion idToken = do
  serverInfo <- retryGet Paths.serverInfoUrl applicationJSON idToken
  return $ E.either (const M.Nothing) M.Just (readProp "version" serverInfo.response)

ldJSON :: MimeType
ldJSON = MimeType "application/ldjson"


-- | Produces a stream of the transitive children of a path
transitiveChildrenProducer
  :: forall e
   . PU.DirPath
  -> M.Maybe Auth.IdToken
  -> CR.Producer
      (Array R.Resource)
      (Aff (RetryEffects (ajax :: AX.AJAX, err :: Exn.EXCEPTION, dom :: DOM | e)))
      Unit
transitiveChildrenProducer dirPath idToken = do
  ACR.produce \emit -> do
    activeRequests <- Ref.newRef $ Set.singleton $ P.printPath dirPath
    Aff.runAff Exn.throwException (const (pure unit)) $ go emit activeRequests dirPath
  where
  go emit activeRequests start = do
    let strPath = P.printPath start
    eitherChildren <- Aff.attempt $ children start idToken
    liftEff $ Ref.modifyRef activeRequests $ Set.delete strPath
    F.for_ eitherChildren \items -> do
      liftEff $ emit $ E.Left items
      let parents = Arr.mapMaybe (E.either (const M.Nothing) M.Just <<< R.getPath) items
      F.for_ parents $ \p ->
        liftEff $ Ref.modifyRef activeRequests $ Set.insert $ P.printPath p
      F.for_ parents $ go emit activeRequests
    remainingRequests <- liftEff $ Ref.readRef activeRequests
    if Set.isEmpty remainingRequests
      then liftEff $ emit $ E.Right unit
      else pure unit

-- | This is template string where actual path is encoded like {{path}}
type SQL = String

query
  :: forall e
   . R.Resource
  -> SQL
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) JS.JArray
query res sql idToken =
  if not $ R.isFile res
  then pure []
  else extractJArray =<< getResponse msg (getOnce uriPath applicationJSON idToken)
  where
  msg = "error in query"
  uriPath = mkURI res sql

query'
  :: forall e
   . R.Resource
  -> SQL
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) (E.Either String JS.JArray)
query' res@(R.File _) sql idToken = do
  result <- retryGet (mkURI' res sql) applicationJSON idToken
  pure if succeeded result.status
       then JS.decodeJson <=< JS.jsonParser $ result.response
       else E.Left $ readError "error in query" result.response

query' _ _ _ = pure $ E.Left "Query resource is not a file"

count
  :: forall e
   . R.Resource
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) Int
count res idToken =
  query res sql idToken
    <#> readTotal
    >>> M.fromMaybe 0
  where
  readTotal :: JS.JArray -> M.Maybe Int
  readTotal =
    Data.Int.fromNumber
      <=< JS.toNumber
      <=< SM.lookup "total"
      <=< JS.toObject
      <=< Arr.head


  uriPath :: P.Path P.Abs P.File P.Sandboxed
  uriPath = mkURI res sql

  sql :: SQL
  sql = "SELECT COUNT(*) as total FROM {{path}}"

resourceExists
  :: forall e
   . R.Resource
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX|e)) Boolean
resourceExists res =
  map E.isRight <<< Aff.attempt <<< count res

portView
  :: forall e
   . R.Resource
  -> R.Resource
  -> SQL
  -> SM.StrMap String
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) Unit
portView res dest sql varMap idToken = do
  guard $ R.isViewMount dest
  let
    queryParams = M.maybe "" ("&" <>) $ renderQueryString varMap
    connectionUri = "sql2:///?q="
                    <> PU.encodeURIPath (templated res sql)
                    <> queryParams
  result <-
    slamjax $ AX.defaultRequest
      { method = PUT
      , headers = [ ContentType applicationJSON ] <> M.maybe [] (\t -> [Auth.authHeader t]) idToken
      , content = M.Just $ stringify { view: { connectionUri: connectionUri } }
      , url =
          P.printPath $
            Paths.mountUrl
              </> PU.rootify (R.resourceDir dest)
              </> P.file (R.resourceName dest)
      }
  if succeeded result.status
     then pure unit
     else Err.throwError $ Exn.error $ readError result.response result.response

portQuery
  :: forall e
   . R.Resource
  -> R.Resource
  -> SQL
  -> SM.StrMap String
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) JS.JObject
portQuery res dest sql vars idToken = do
  guard $ R.isFile dest
  result <-
    slamjax $ AX.defaultRequest
      { method = POST
      , headers =
          [ RequestHeader "Destination" $ R.resourcePath dest
          , ContentType ldJSON
          ] <> M.maybe [] (\t -> [Auth.authHeader t]) idToken
      , url =
          P.printPath $
            Paths.queryUrl
              </> PU.rootify (R.resourceDir res)
              </> P.dir (R.resourceName res)
              </> P.file queryVars
      , content = M.Just (templated res sql)
      }

  if not $ succeeded result.status
    then Err.throwError $ Exn.error $ readError result.response result.response
    else
    -- We expect result message to be valid json.
    E.either (Err.throwError <<< Exn.error) pure $
      JS.jsonParser result.response >>= JS.decodeJson
  where
  queryVars :: String
  queryVars = M.maybe "" ("?" <>) $ renderQueryString vars

renderQueryString :: SM.StrMap String -> M.Maybe String
renderQueryString = map go <<< L.uncons <<< SM.toList
  where
  pair :: Tuple String String -> String
  pair (Tuple a b) = "var." <> a <> "=" <> encodeURIComponent b

  go { head = h, tail = t } =
    F.foldl (\a v -> a <> "&" <> pair v) (pair h) t


readError :: String -> String -> String
readError msg input =
  let responseError = JS.jsonParser input >>= JS.decodeJson >>= (.? "error")
  in E.either (const msg) id responseError

sample'
  :: forall e
   . R.Resource
  -> M.Maybe Int
  -> M.Maybe Int
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) JS.JArray
sample' res mbOffset mbLimit idToken =
  if not $ R.isFile res
  then pure []
  else extractJArray =<< getResponse msg (retryGet uri applicationJSON idToken)
  where
  msg = "error getting resource sample"
  uri =
    Paths.dataUrl
      </> PU.rootify (R.resourceDir res)
      </> P.file
            (R.resourceName res
               <> (M.maybe "" (("?offset=" <>) <<< show) mbOffset)
               <> (M.maybe "" (("&limit=" <>) <<< show ) mbLimit))


sample
  :: forall e
   . R.Resource
  -> Int
  -> Int
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) JS.JArray
sample res offset limit =
  sample' res (M.Just offset) (M.Just limit)

all
  :: forall e
   . R.Resource
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) JS.JArray
all res =
  sample' res M.Nothing M.Nothing

fields
  :: forall e
   . R.Resource
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) (Array String)
fields res idToken = do
  jarr <- sample res 0 100 idToken
  case jarr of
    [] -> Err.throwError $ Exn.error "empty file"
    _ -> pure $ Arr.nub $ Arr.concat (getFields <$> jarr)

mkURI :: R.Resource -> SQL -> PU.FilePath
mkURI res sql =
  Paths.queryUrl </> P.file ("?q=" <> encodeURIComponent (templated res sql))

mkURI' :: R.Resource -> SQL -> PU.FilePath
mkURI' res sql =
  Paths.queryUrl
  </> PU.rootify (R.resourceDir res)
  </> P.dir (R.resourceName res)
  </> P.file ("?q=" <> encodeURIComponent (templated res sql))

templated :: R.Resource -> SQL -> SQL
templated res = S.replace "{{path}}" ("\"" <> R.resourcePath res <> "\"")

extractJArray :: forall m. (Err.MonadError Exn.Error m) => JS.Json -> m JS.JArray
extractJArray = E.either (Err.throwError <<< Exn.error) pure <<< JS.decodeJson

getFields :: JS.Json -> Array String
getFields json = Arr.filter (/= "") $ Arr.nub $ getFields' [] json

getFields' :: Array String -> JS.Json -> Array String
getFields' [] json = getFields' [""] json
getFields' acc json =
  if JS.isObject json
  then M.maybe acc (goObj acc) $ JS.toObject json
  else if JS.isArray json
       then M.maybe acc (goArr acc) $ JS.toArray json
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
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX, dom :: DOM | e))
      (E.Either String { outputResource :: R.Resource, plan :: M.Maybe String })
executeQuery sql cachingEnabled varMap inputResource outputResource idToken = do
  when (R.isTempFile outputResource)
    $ void $ Aff.attempt $ forceDelete outputResource idToken

  ejobj <- do
    Aff.attempt $
      if cachingEnabled
         then portQuery inputResource outputResource sql varMap idToken <#> M.Just
         else portView inputResource outputResource sql varMap idToken $> M.Nothing
  pure $ do
    mjobj <- lmap Exn.message ejobj
    info <-
      case mjobj of
        M.Nothing -> do
          path <- R.getPath outputResource # E.either pure \_ ->
            E.Left "Expected output resource as file or view mount"
          sandboxedPath <-
            P.sandbox P.rootDir path
              # M.maybe (E.Left "Could not sandbox output resource") pure
          pure
            { sandboxedPath
            , plan: M.Nothing
            }
        M.Just jobj -> do
          planPhases <- Arr.last <$> jobj .? "phases"
          sandboxedPath <- do
            pathString <- jobj .? "out"
            path <- P.parseAbsFile pathString
                    # M.maybe (E.Left "Invalid file from Quasar") pure
            P.sandbox P.rootDir path
              # M.maybe (E.Left "Could not sandbox Quasar file") pure
          pure
            { sandboxedPath
            , plan: planPhases >>= (.? "detail") >>> E.either (const M.Nothing) M.Just
            }
    pure
      { outputResource: R.mkFile $ E.Left $ P.rootDir </> info.sandboxedPath
      , plan: info.plan
      }

-- | Saves a JSON value to a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
save
  :: forall e
   . PU.FilePath
  -> JS.Json
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) Unit
save path json idToken =
  let apiPath = Paths.dataUrl </> PU.rootifyFile path
  in getResponse "error while saving file" (retryPut apiPath json ldJSON idToken)

-- | Loads a JSON value from a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
load
  :: forall e
   . PU.FilePath
  -> M.Maybe Auth.IdToken
  -> Aff (RetryEffects (ajax :: AX.AJAX | e)) (E.Either String JS.Json)
load path idToken =
  let apiPath = Paths.dataUrl </> PU.rootifyFile path
  in lmap Exn.message <$> Aff.attempt (getResponse "error loading notebook" (retryGet apiPath ldJSON idToken))
