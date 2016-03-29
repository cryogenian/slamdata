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
  , query' -- TODO: rename this, we shouldn't be exporting things with inscruatable prime symbols
  , queryPrecise
  , count

  , save
  , load
  , messageIfResourceNotExists
  , portView

  , retrieveAuthProviders
  , compile
  , RetryEffects
  , encodeURI
  ) where

import SlamData.Prelude

import Control.Apply (lift2)
import Control.Coroutine as CR
import Control.Coroutine.Aff as ACR
import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Error.Class as Err
import Control.UI.Browser (encodeURIComponent)

import Data.Argonaut ((~>), (:=), (.?))
import Data.Argonaut as JS
import Data.Array as Arr
import Data.Date as Date
import Data.Foreign (F, parseJSON)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.Index (prop)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
import Data.HTTP.Method (Method(..))
import Data.Lens ((.~), (^.))
import Data.List as L
import Data.MediaType (MediaType(..), mediaTypeToString)
import Data.MediaType.Common (applicationJSON)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as Set
import Data.String as S
import Data.StrMap as SM
import Data.Time as Time
import Data.URI (runParseAbsoluteURI) as URI
import Data.URI.Types (AbsoluteURI(..), Query(..), URIScheme(..)) as URI

import DOM (DOM)

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))

import Quasar.Paths as Paths
import Quasar.Auth (IdToken, authHeader) as Auth
import Quasar.Auth.Permission as Perm
import Quasar.Auth.Provider (Provider) as Auth

-- TODO: split out a core Quasar module that only deals with the API, and
-- doesn't know about SlamData specific things.
import SlamData.Config as Config
import SlamData.FileSystem.Resource as R

import Utils.Completions (memoizeCompletionStrs)
import Utils.Path as PU

successStatus ∷ StatusCode
successStatus = StatusCode 200

notFoundStatus ∷ StatusCode
notFoundStatus = StatusCode 404

succeeded ∷ StatusCode → Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = int

type RetryEffects e = (avar ∷ AVar.AVAR, ref ∷ Ref.REF, now ∷ Date.Now | e)

encodeURI ∷ String → String
encodeURI str =
  let
    encode ∷ String → String
    encode = encodeURIComponent

    qmSplitted ∷ Array String
    qmSplitted = S.split "?" str

    ampSplitted ∷ Maybe (Array String)
    ampSplitted = map (S.split "&") $ qmSplitted Arr.!! 1

    eqSplitted ∷ Maybe (Array (Array String))
    eqSplitted = map (map $ S.split "=") $ ampSplitted

    maybeModify ∷ ∀ a. Int → (a → a) → Array a → Array a
    maybeModify ix fn arr =
      fromMaybe arr $ Arr.modifyAt ix fn arr

    eqSplittedEncoded ∷ Maybe (Array (Array String))
    eqSplittedEncoded = map (map (maybeModify 1 encode)) eqSplitted

    eqMerged ∷ Maybe (Array String)
    eqMerged = map (map (S.joinWith "=")) eqSplittedEncoded

    ampMerged ∷ Maybe String
    ampMerged = map (S.joinWith "&") eqMerged

    slashSplitted ∷ Maybe (Array String)
    slashSplitted = map (S.split "/") $ Arr.head qmSplitted

    slashSplittedEncoded ∷ Maybe (Array String)
    slashSplittedEncoded = map (map encode) $ slashSplitted

    slashMerged ∷ Maybe String
    slashMerged = map (S.joinWith "/") slashSplittedEncoded

    afterQM ∷ String
    afterQM = foldMap ("?" ⊕ _) ampMerged

    beforeQM ∷ String
    beforeQM = fromMaybe "" slashMerged

  in
    beforeQM ⊕ afterQM

-- | A version of `affjax` with our retry policy.
slamjax
  ∷ forall e a b
  . (Requestable a, Respondable b)
  ⇒ AX.AffjaxRequest a
  → AX.Affjax (RetryEffects e) b
slamjax =
  AX.retry
    AX.defaultRetryPolicy
    AX.affjax

retryGet
  ∷ forall e a fd
  . (Respondable a)
  ⇒ P.Path P.Abs fd P.Sandboxed
  → MediaType
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → AX.Affjax (RetryEffects e) a
retryGet =
  getWithPolicy $ AX.defaultRetryPolicy { delayCurve = const 1000 }

mkRequest
  ∷ forall e fd
  . P.Path P.Abs fd P.Sandboxed
  → MediaType
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects e) (AX.AffjaxRequest Unit)
mkRequest u mime idToken perms = do
  nocache ← liftEff $ Date.nowEpochMilliseconds
  pure
    $ insertAuthHeaders idToken perms
    $ AX.defaultRequest
      { url = encodeURI $ url' nocache
      , headers = [ Accept mime ]
      }
  where
  url' nocache = url ⊕ symbol ⊕ "nocache=" ⊕ pretty nocache
  symbol = if S.contains "?" url then "&" else "?"
  pretty (Time.Milliseconds ms) = let s = show ms in fromMaybe s (S.stripSuffix ".0" s)
  url = P.printPath u

getOnce
  ∷ forall e a fd
  . (Respondable a)
  ⇒ P.Path P.Abs fd P.Sandboxed
  → MediaType
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → AX.Affjax (RetryEffects e) a
getOnce u mime idToken perms =
  mkRequest u mime idToken perms
    >>= AX.affjax

getWithPolicy
  ∷ forall e a fd
  . (Respondable a)
  ⇒ AX.RetryPolicy
  → P.Path P.Abs fd P.Sandboxed
  → MediaType
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → AX.Affjax (RetryEffects e) a
getWithPolicy policy u mime idToken perms =
  mkRequest u mime idToken perms
    >>= AX.retry policy AX.affjax

retryDelete
  ∷ forall e a fd
  . (Respondable a)
  ⇒ P.Path P.Abs fd P.Sandboxed
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → AX.Affjax (RetryEffects e) a
retryDelete u idToken perms = do
  slamjax
    $ insertAuthHeaders idToken perms
    $ AX.defaultRequest
      { url = encodeURI $ P.printPath u
      , method = Left DELETE
      }

retryPut
  ∷ forall e a b fd
  . (Requestable a, Respondable b)
  ⇒ P.Path P.Abs fd P.Sandboxed
  → a
  → MediaType
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → AX.Affjax (RetryEffects e) b
retryPut u c mime idToken perms =
  slamjax
    $ insertAuthHeaders idToken perms
    $ AX.defaultRequest
      { method = Left PUT
      , url = encodeURI $ P.printPath u
      , content = Just c
      , headers = [ContentType mime]
      }

getResponse ∷ forall a e. String → AX.Affjax e a → Aff (ajax ∷ AX.AJAX | e) a
getResponse msg m = do
  res ← Aff.attempt m
  case res of
    Left e → Err.throwError $ Exn.error msg
    Right r → do
      if not $ succeeded r.status
        then Err.throwError $ Exn.error msg
        else pure r.response

reqHeadersToJSON ∷ ∀ f. Foldable f ⇒ f RequestHeader → JS.Json
reqHeadersToJSON = foldl go JS.jsonEmptyObject
  where
  go obj (Accept mime) = "Accept" := mediaTypeToString mime ~> obj
  go obj (ContentType mime) = "Content-Type" := mediaTypeToString mime ~> obj
  go obj (RequestHeader k v) = k := v ~> obj

mkURI ∷ R.Resource → SQL → PU.FilePath
mkURI res sql =
  Paths.queryUrl </> P.file ("?q=" ⊕ templated res sql)

mkURI' ∷ R.Resource → SQL → PU.FilePath
mkURI' res sql =
  Paths.queryUrl
  </> PU.rootify (R.resourceDir res)
  </> P.dir (R.resourceName res)
  </> P.file ("?q=" ⊕ templated res sql)

templated ∷ R.Resource → SQL → SQL
templated res = S.replace "{{path}}" ("`" ⊕ R.resourcePath res ⊕ "`")


newtype Listing = Listing (Array R.Resource)

runListing ∷ Listing → Array R.Resource
runListing (Listing rs) = rs

instance listingIsForeign ∷ IsForeign Listing where
  read f =
    read f
      >>= readProp "children"
      >>= pure
        ∘ Listing
        ∘ fromMaybe []
        ∘ runNullOrUndefined

instance listingRespondable ∷ Respondable Listing where
  responseType = (Just applicationJSON) × JSONResponse
  fromResponse = read

insertAuthHeaders
  ∷ forall a
  . Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → AX.AffjaxRequest a
  → AX.AffjaxRequest a
insertAuthHeaders mbToken perms r =
  r { headers =
        r.headers
        ⊕ (foldMap (pure ∘ Auth.authHeader) mbToken)
        ⊕ (foldMap pure $ Perm.permissionsHeader perms)
    }



children
  ∷ forall e
  . PU.DirPath
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX, dom ∷ DOM | e)) (Array R.Resource)
children dir idToken perms = do
  cs ← children' dir idToken perms
  let result = (R._root .~ dir) <$> cs
  memoizeCompletionStrs dir result
  pure result

children'
  ∷ forall e
  . PU.DirPath
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) (Array R.Resource)
children' dir idToken perms =
  listing dir idToken perms
    # getResponse msg
    <#> runListing
  where
  msg = "Error: can not get children of resource"

listing
  ∷ forall e
  . PU.DirPath
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → AX.Affjax (RetryEffects e) Listing
listing p idToken perms =
  case P.relativeTo p P.rootDir of
    Nothing → Err.throwError $ Exn.error "incorrect path"
    Just p →
      retryGet
        (Paths.metadataUrl </> p)
        applicationJSON
        idToken
        perms

makeFile
  ∷ forall e
  . PU.FilePath
  → MediaType
  → String
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) Unit
makeFile path mime content idToken perms =
  getResponse "error while creating file"
    $ slamjax
    $ insertAuthHeaders idToken perms
    $ AX.defaultRequest
      { method = Left PUT
      , headers = [ ContentType mime ]
      , content = Just content
      , url =
          encodeURI
          $ fromMaybe ""
          $ (P.printPath ∘ (Paths.dataUrl </> _))
          <$> P.relativeTo path P.rootDir
      }

mountInfo
  ∷ forall e
  . PU.DirPath
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) String
mountInfo res idToken perms = do
  result ← getOnce mountPath applicationJSON idToken perms
  if succeeded result.status
     then case parse result.response of
       Left err → Err.throwError $ Exn.error (show err)
       Right uri → pure uri
     else Err.throwError (Exn.error result.response)

  where
  mountPath ∷ P.Path P.Abs P.Dir P.Sandboxed
  mountPath = Paths.mountUrl </> PU.rootify res

  parse ∷ String → F String
  parse = parseJSON >=> prop "mongodb" >=> readProp "connectionUri"

viewInfo
  ∷ forall e
  . PU.FilePath
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX|e))
     { query ∷ String, vars ∷ SM.StrMap String }
viewInfo mountPath idToken perms = do
  result ←
    getOnce (Paths.mountUrl </> PU.rootifyFile mountPath)
      applicationJSON idToken perms
  if succeeded result.status
    then case parse result.response of
      Left err → Err.throwError $ Exn.error err
      Right res → pure res
    else Err.throwError $ Exn.error result.response
  where
  runQuery ∷ URI.Query → SM.StrMap (Maybe String)
  runQuery (URI.Query q) = q

  parse ∷ String → Either String { query ∷ String, vars ∷ SM.StrMap String }
  parse connURI = do
    connStr ←
      lmap show
      $ parseJSON connURI
      >>= prop "view"
      >>= readProp "connectionUri"
    URI.AbsoluteURI mbScheme _ mbQuery ← lmap show $ URI.runParseAbsoluteURI connStr
    scheme ← maybe (Err.throwError "There is no scheme") pure mbScheme
    unless (scheme ≡ URI.URIScheme "sql2") $ Err.throwError "Incorrect scheme"

    let queryMap = maybe SM.empty runQuery mbQuery
    sql ←
      maybe (Err.throwError "There is no 'q' in queryMap") pure
        $ SM.lookup "q" queryMap
        >>= id
        ⋙ map PU.decodeURIPath
        >>= S.stripPrefix "("
        >>= S.stripSuffix ")"
    let vars = SM.fold foldFn SM.empty $ SM.delete "q" queryMap
    pure { query: sql, vars }
    where
    foldFn ∷ SM.StrMap String → String → Maybe String → SM.StrMap String
    foldFn acc key mbVal = fromMaybe acc do
      k ← S.stripPrefix "var." key
      val ← mbVal
      pure $ SM.insert k val acc


-- | Generates a new resource name based on a directory path and a name for the
-- | resource. If the name already exists in the path a number is appended to
-- | the end of the name.
getNewName
  ∷ forall e
  . PU.DirPath
  → String
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX |e)) String
getNewName parent name idToken perms = do
  items ← Aff.attempt (children' parent idToken perms) <#> either (const []) id
  pure if exists' name items then getNewName' items 1 else name
  where
  getNewName' items i =
    let arr = S.split "." name
    in fromMaybe "" do
      body ← Arr.head arr
      suffixes ← Arr.tail arr
      let newName = S.joinWith "." $ Arr.cons (body ⊕ " " ⊕ show i) suffixes
      pure if exists' newName items
           then getNewName' items (i + one)
           else newName

exists' ∷ String → Array R.Resource → Boolean
exists' name items = isJust $ Arr.findIndex (\r → r ^. R._name ≡ name) items

-- | Will return `Just` in case the resource was successfully moved, and
-- | `Nothing` in case no resource existed at the requested source path.
move
  ∷ forall e
  . R.Resource
  → PU.AnyPath
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX, dom ∷ DOM |e)) (Maybe PU.AnyPath)
move src tgt idToken perms = do
  let
    url = if R.isMount src
          then Paths.mountUrl
          else Paths.dataUrl
    resourceUrl =
      either
        (P.printPath ∘ (url </> _) ∘ PU.rootifyFile)
        (P.printPath ∘ (url </> _) ∘ PU.rootify)
        (R.getPath src)
    queryPart =
      "?request-headers="
      ⊕ (show
         $ reqHeadersToJSON
           [RequestHeader "Destination"
            $ either P.printPath P.printPath tgt])

  cleanViewMounts src idToken perms
  result ←
    AX.affjax
    $ insertAuthHeaders idToken perms
    $ AX.defaultRequest
      { method = Left MOVE
      , url = encodeURI $ resourceUrl ⊕ queryPart
      }
  if succeeded result.status
    then pure $ Just tgt
    else if result.status ≡ notFoundStatus
      then pure Nothing
      else Err.throwError (Exn.error result.response)

saveMount
  ∷ forall e
  . PU.DirPath
  → String
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX |e)) Unit
saveMount path uri idToken perms = do
  result ←
    slamjax
    $ insertAuthHeaders idToken perms
    $ AX.defaultRequest
      { method = Left PUT
      , headers = [ ContentType applicationJSON ]
      , content = Just $ stringify { mongodb: {connectionUri: uri } }
      , url = encodeURI $ P.printPath $ Paths.mountUrl </> PU.rootify path
      }
  if succeeded result.status
    then pure unit
    else Err.throwError (Exn.error result.response)

foreign import stringify ∷ forall r. {|r} → String

delete
  ∷ forall e
  . R.Resource
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX, dom ∷ DOM |e)) (Maybe R.Resource)
delete resource idToken perms =
  if not (R.isMount resource || alreadyInTrash resource)
  then (moveToTrash resource) <|> (forceDelete resource idToken perms $> Nothing)
  else forceDelete resource idToken perms $> Nothing

  where
  msg ∷ String
  msg = "cannot delete"

  moveToTrash
    ∷ R.Resource
    → Aff (RetryEffects (ajax ∷ AX.AJAX, dom ∷ DOM | e)) (Maybe R.Resource)
  moveToTrash res = do
    let d = (res ^. R._root) </> P.dir Config.trashFolder
        path = (res # R._root .~ d) ^. R._path
    name ← getNewName d (res ^. R._name) idToken perms
    move res (path # R._nameAnyPath .~ name) idToken perms
    pure $ Just $ R.Directory d

  alreadyInTrash ∷ R.Resource → Boolean
  alreadyInTrash res =
    case res ^. R._path of
      Left _ → alreadyInTrash' (res ^. R._root)
      Right path → alreadyInTrash' path

  alreadyInTrash' ∷ PU.DirPath → Boolean
  alreadyInTrash' d =
    if d ≡ P.rootDir
    then false
    else maybe false go $ P.peel d

    where
    go ∷ Tuple PU.DirPath (Either P.DirName P.FileName) → Boolean
    go (d × name) =
      case name of
        Right _ → false
        Left n →
          if n ≡ P.DirName Config.trashFolder
          then true
          else alreadyInTrash' d


forceDelete
  ∷ forall e
  . R.Resource
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX, dom ∷ DOM |e)) Unit
forceDelete res idToken perms = do
  cleanViewMounts res idToken perms
  getResponse "cannot delete"
    $ either
      (\x → retryDelete x idToken perms)
      (\y → retryDelete y idToken perms)
    $ pathFromResource res

  where
  pathFromResource ∷ R.Resource → PU.AnyPath
  pathFromResource r = transplant (rootForResource r) (R.getPath r)

  transplant ∷ PU.DirPath → PU.AnyPath → PU.AnyPath
  transplant newRoot =
    bimap
      (\p → newRoot </> PU.rootifyFile p)
      (\p → newRoot </> PU.rootify p)

  rootForResource ∷ R.Resource → PU.DirPath
  rootForResource r = if R.isMount r then Paths.mountUrl else Paths.dataUrl

cleanViewMounts
  ∷ forall e
  . R.Resource
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX, dom ∷ DOM|e)) Unit
cleanViewMounts res idToken perms =
  for_ (R.getPath res) \dirPath →
    children dirPath idToken perms
      >>= Arr.filter R.isViewMount
      ⋙ traverse_ (\x → forceDelete x idToken perms)

getVersion
  ∷ forall e
  . Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX |e)) (Maybe String)
getVersion idToken perms = do
  serverInfo ← retryGet Paths.serverInfoUrl applicationJSON idToken perms
  return $ either (const Nothing) Just (readProp "version" serverInfo.response)

ldJSON ∷ MediaType
ldJSON = MediaType "application/ldjson"

preciseJSON ∷ MediaType
preciseJSON = MediaType "application/json;mode=precise"


-- | Produces a stream of the transitive children of a path
transitiveChildrenProducer
  ∷ forall e
  . PU.DirPath
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → CR.Producer
      (Array R.Resource)
      (Aff (RetryEffects (ajax ∷ AX.AJAX, err ∷ Exn.EXCEPTION, dom ∷ DOM | e)))
      Unit
transitiveChildrenProducer dirPath idToken perms = do
  ACR.produce \emit → do
    activeRequests ← Ref.newRef $ Set.singleton $ P.printPath dirPath
    Aff.runAff Exn.throwException (const (pure unit)) $ go emit activeRequests dirPath
  where
  go emit activeRequests start = do
    let strPath = P.printPath start
    eitherChildren ← Aff.attempt $ children start idToken perms
    liftEff $ Ref.modifyRef activeRequests $ Set.delete strPath
    for_ eitherChildren \items → do
      liftEff $ emit $ Left items
      let parents = Arr.mapMaybe (either (const Nothing) Just ∘ R.getPath) items
      for_ parents $ \p →
        liftEff $ Ref.modifyRef activeRequests $ Set.insert $ P.printPath p
      for_ parents $ go emit activeRequests
    remainingRequests ← liftEff $ Ref.readRef activeRequests
    if Set.isEmpty remainingRequests
      then liftEff $ emit $ Right unit
      else pure unit

-- | This is template string where actual path is encoded like {{path}}
type SQL = String

query
  ∷ forall e
  . R.Resource
  → SQL
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) JS.JArray
query res sql idToken perms =
  if (not $ R.isFile res) && (not $ R.isViewMount res)
  then pure []
  else extractJArray
         =<< getResponse msg (getOnce uriPath applicationJSON idToken perms)
  where
  msg = "error in query"
  uriPath = mkURI res sql

query'
  ∷ forall e
  . PU.FilePath
  → SQL
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) (Either String JS.JArray)
query' path sql idToken perms = do
  let res = R.File path
  eResult ← Aff.attempt $
    AX.affjax =<< mkRequest (mkURI' res sql) applicationJSON idToken perms
  pure
    case eResult of
      Left err → Left (Exn.message err)
      Right result →
        if succeeded result.status
        then JS.decodeJson <=< JS.jsonParser $ result.response
        else Left $ readError "error in query" result.response

queryPrecise
  ∷ forall e
  . PU.FilePath
  → SQL
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) (Either String JS.JArray)
queryPrecise path sql idToken perms = do
  let res = R.File path
  eResult ← Aff.attempt $
    AX.affjax =<< mkRequest (mkURI' res sql) preciseJSON idToken perms
  pure
    case eResult of
      Left err → Left (Exn.message err)
      Right result →
        if succeeded result.status
        then JS.decodeJson <=< JS.jsonParser $ result.response
        else Left $ readError "error in query" result.response

count
  ∷ forall e
  . R.Resource
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) Int
count res idToken perms =
  query res sql idToken perms
    <#> readTotal
    ⋙ fromMaybe 0
  where
  readTotal ∷ JS.JArray → Maybe Int
  readTotal =
    Data.Int.fromNumber
      <=< JS.toNumber
      <=< SM.lookup "total"
      <=< JS.toObject
      <=< Arr.head


  uriPath ∷ P.Path P.Abs P.File P.Sandboxed
  uriPath = mkURI res sql

  sql ∷ SQL
  sql = "SELECT COUNT(*) as total FROM {{path}}"

messageIfResourceNotExists
  ∷ forall e
  . R.Resource
  → String
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX|e)) (Maybe String)
messageIfResourceNotExists res defaultMsg idToken perms = do
  eResult ← Aff.attempt existsReq
  pure case eResult of
    Left e → Just $ Exn.message e
    Right result →
      guard (result.status ≠ successStatus)
      $> if result.status ≡ notFoundStatus
           then defaultMsg
           else "Unexpected status code " ⊕ show result.status
  where
  requestPath
    ∷ P.Path P.Abs P.File P.Sandboxed
  requestPath | R.isViewMount res =
    Paths.dataUrl
    </> PU.rootify (R.resourceDir res)
    </> P.file (R.resourceName res ⊕ "?offset=0&limit=10")
  requestPath =
    Paths.metadataUrl
    </> PU.rootify (R.resourceDir res)
    </> P.file (R.resourceName res)

  existsReq
    ∷ Aff (RetryEffects (ajax ∷ AX.AJAX|e)) (AX.AffjaxResponse Unit)
  existsReq =
    getOnce requestPath applicationJSON idToken perms


portView
  ∷ forall e
  . R.Resource
  → R.Resource
  → SQL
  → SM.StrMap String
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) Unit
portView res dest sql varMap idToken perms = do
  guard $ R.isViewMount dest
  let
    queryParams = foldMap ("&" ⊕ _) $ renderQueryString varMap
    connectionUri = "sql2:///?q="
                    ⊕ (encodeURIComponent $ templated res sql)
                    ⊕ queryParams
  result ←
    AX.affjax
    $ insertAuthHeaders idToken perms
    $ AX.defaultRequest
      { method = Left PUT
      , headers = [ ContentType applicationJSON ]
      , content = Just $ stringify { view: { connectionUri: connectionUri } }
      , url =
          encodeURI
          $ P.printPath
          $ Paths.mountUrl
          </> PU.rootify (R.resourceDir dest)
          </> P.file (R.resourceName dest)
      }
  if succeeded result.status
     then pure unit
     else Err.throwError $ Exn.error $ readError result.response result.response

portQuery
  ∷ forall e
  . R.Resource
  → R.Resource
  → SQL
  → SM.StrMap String
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) JS.JObject
portQuery res dest sql vars idToken perms = do
  guard $ R.isFile dest
  let
    resourceUrl =
      P.printPath
        $ Paths.queryUrl
        </> PU.rootify (R.resourceDir res)
        </> P.dir (R.resourceName res)
    headerPart =
      "?request-headers="
      ⊕ (show
         $ reqHeadersToJSON
             [ RequestHeader "Destination"
                 $ R.resourcePath dest
             , ContentType ldJSON
             ])

    varMapPart =
      foldMap ("&" ⊕ _) $ renderQueryString vars
  result ←
    AX.affjax
      $ insertAuthHeaders idToken perms
      $ AX.defaultRequest
        { method = Left POST
        , url = encodeURI $ resourceUrl ⊕ headerPart ⊕ varMapPart
        , content = Just (templated res sql)
        }

  if not $ succeeded result.status
    then Err.throwError $ Exn.error $ readError result.response result.response
    else
    -- We expect result message to be valid json.
    either (Err.throwError ∘ Exn.error) pure $
      JS.jsonParser result.response >>= JS.decodeJson

renderQueryString ∷ SM.StrMap String → Maybe String
renderQueryString = map go ∘ L.uncons ∘ SM.toList
  where
  pair ∷ Tuple String String → String
  pair (a × b) = "var." ⊕ a ⊕ "=" ⊕ encodeURIComponent b

  go { head = h, tail = t } =
    foldl (\a v → a ⊕ "&" ⊕ pair v) (pair h) t


readError ∷ String → String → String
readError msg input =
  let responseError = JS.jsonParser input >>= JS.decodeJson >>= (_ .? "error")
  in either (const msg) id responseError

sample'
  ∷ forall e
  . R.Resource
  → Maybe Int
  → Maybe Int
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) JS.JArray
sample' res mbOffset mbLimit idToken perms =
  if (not $ R.isFile res) && (not $ R.isViewMount res)
    then pure []
    else extractJArray
           =<< getResponse msg (retryGet uri applicationJSON idToken perms)
  where
  msg = "error getting resource sample"
  uri =
    Paths.dataUrl
      </> PU.rootify (R.resourceDir res)
      </> P.file
            (R.resourceName res
               ⊕ (foldMap (("?offset=" ⊕ _) ∘ show) mbOffset)
               ⊕ (foldMap (("&limit=" ⊕ _) ∘ show ) mbLimit))


sample
  ∷ forall e
  . R.Resource
  → Int
  → Int
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) JS.JArray
sample res offset limit =
  sample' res (Just offset) (Just limit)

all
  ∷ forall e
  . R.Resource
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) JS.JArray
all res =
  sample' res Nothing Nothing

fields
  ∷ forall e
  . R.Resource
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) (Array String)
fields res idToken perms = do
  jarr ← sample res 0 100 idToken perms
  case jarr of
    [] → Err.throwError $ Exn.error "empty file"
    _ → pure $ Arr.nub $ getFields =<< jarr


extractJArray ∷ forall m. (Err.MonadError Exn.Error m) ⇒ JS.Json → m JS.JArray
extractJArray = either (Err.throwError ∘ Exn.error) pure ∘ JS.decodeJson

-- The output of this function is mysterious, but luckily is used in just one place.
--
-- TODO: Rather than accumulating a an array of formatted strings, this should be refactored
-- to return an array of *arrays* of unformatted strings, which can then be formatted by the
-- client (e.g. to intercalate with dots and add backticks).
getFields ∷ JS.Json → Array String
getFields = Arr.filter (_ ≠ "") ∘ Arr.nub ∘ go []
  where
  go ∷ Array String → JS.Json → Array String
  go [] json = go [""] json
  go acc json =
    if JS.isObject json
    then maybe acc (goObj acc) $ JS.toObject json
    else if JS.isArray json
         then maybe acc (goArr acc) $ JS.toArray json
         else acc

    where
    goArr ∷ Array String → JS.JArray → Array String
    goArr acc arr =
      Arr.concat $ go (lift2 append acc $ mkArrIxs arr) <$> arr
      where
      mkArrIxs ∷ JS.JArray → Array String
      mkArrIxs jarr =
        map (show ⋙ \x → "[" ⊕ x ⊕ "]") $ Arr.range 0 $ Arr.length jarr - 1

    goObj ∷ Array String → JS.JObject → Array String
    goObj acc = Arr.concat ∘ map (goTuple acc) ∘ L.fromList ∘ SM.toList

    goTuple ∷ Array String → Tuple String JS.Json → Array String
    goTuple acc (key × json) =
      go ((\x → x ⊕ ".`" ⊕ key ⊕ "`") <$> acc) json

executeQuery
  ∷ forall e
  . String
  → Boolean
  → SM.StrMap String
  → R.Resource
  → R.Resource
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX, dom ∷ DOM | e))
      (Either String { outputResource ∷ R.Resource, plan ∷ Maybe String })
executeQuery sql cachingEnabled varMap inputResource outputResource idToken perms = do
  when (R.isTempFile outputResource)
    $ void $ Aff.attempt $ forceDelete outputResource idToken perms

  compiledPlan ←
    Aff.attempt $ compile sql inputResource varMap idToken perms

  ejobj ← do
    Aff.attempt
      $ if cachingEnabled
        then
          portQuery inputResource outputResource sql varMap idToken perms <#> Just
        else
          portView inputResource outputResource sql varMap idToken perms $> Nothing
  pure do
    mjobj ← lmap Exn.message ejobj
    info ←
      case mjobj of
        Nothing → do
          path ← R.getPath outputResource # either pure \_ →
            Left "Expected output resource as file or view mount"
          sandboxedPath ←
            P.sandbox P.rootDir path
              # maybe (Left "Could not sandbox output resource") pure
          pure
            { sandboxedPath
            , plan: either (\_ → Nothing) Just compiledPlan
            }
        Just jobj → do
          planPhases ← Arr.last <$> jobj .? "phases"
          sandboxedPath ← do
            pathString ← jobj .? "out"
            path ← P.parseAbsFile pathString
                    # maybe (Left "Invalid file from Quasar") pure
            P.sandbox P.rootDir path
              # maybe (Left "Could not sandbox Quasar file") pure
          pure
            { sandboxedPath
            , plan: planPhases >>= (_ .? "detail") ⋙ either (const Nothing) Just
            }
    pure
      { outputResource: mkOutputResource $ P.rootDir </> info.sandboxedPath
      , plan: info.plan
      }
  where
  mkOutputResource | cachingEnabled = R.File
  mkOutputResource = R.Mount ∘ R.View

-- | Saves a JSON value to a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
save
  ∷ forall e
  . PU.FilePath
  → JS.Json
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) Unit
save path json idToken perms =
  let apiPath = Paths.dataUrl </> PU.rootifyFile path
  in getResponse "error while saving file"
       (retryPut apiPath json ldJSON idToken perms)

-- | Loads a JSON value from a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
load
  ∷ forall e
  . PU.FilePath
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX | e)) (Either String JS.Json)
load path idToken perms =
  let apiPath = Paths.dataUrl </> PU.rootifyFile path
  in lmap Exn.message <$> Aff.attempt (getResponse "error loading notebook" (retryGet apiPath ldJSON idToken perms))

-- | Returns `Nothing` in case the authorization service is not available, and `Just` in case
-- | Quasar responded with a valid array of OIDC providers.
retrieveAuthProviders
  ∷ forall e
  . Aff (RetryEffects (ajax ∷ AX.AJAX | e)) (Maybe (Array Auth.Provider))
retrieveAuthProviders = do
  res ← getOnce Paths.oidcProvidersUrl applicationJSON Nothing []
  if res.status ≡ notFoundStatus
    then pure Nothing
    else do
    case JS.decodeJson res.response of
      Left parseErr → Err.throwError $ Exn.error parseErr
      Right val → pure $ Just val


compile
  ∷ forall e
  . String
  → R.Resource
  → SM.StrMap String
  → Maybe Auth.IdToken
  → Array Perm.PermissionToken
  → Aff (RetryEffects (ajax ∷ AX.AJAX|e)) String
compile sql res varMap idToken perms = do
  result ←
    getOnce path applicationJSON idToken perms
  if not $ succeeded result.status
    then Err.throwError $ Exn.error $ readError result.response result.response
    else case S.stripPrefix "MongoDB\n" result.response of
      Nothing → Err.throwError $ Exn.error "Incorrect compile response"
      Just plan → pure plan
  where
  path =
    Paths.compileUrl
    </> PU.rootify (R.resourceDir res)
    </> P.dir (R.resourceName res)
    </> P.file ("?q=" ⊕ templated res sql ⊕ queryVars)
  queryVars ∷ String
  queryVars = maybe "" ("&" ⊕ _) $ renderQueryString varMap
