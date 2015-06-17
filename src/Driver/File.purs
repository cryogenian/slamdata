-- | Module handles outer messages to `halogen` application
-- | Mostly consists of routing functions
module Driver.File (outside) where

import Api.Fs (children, mountInfo)
import Control.Monad.Aff (launchAff, cancel, attempt, Canceler(), Aff(), forkAff)
import Control.Monad.Aff.AVar (makeVar', takeVar, putVar, modifyVar, AVar(), AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, message)
import Controller.File.Common (showError, browseURL)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (foldl, traverse_)
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Path.Pathy ((</>), rootDir, currentDir, parseAbsDir, sandbox)
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Driver.File.Path (renderPath)
import Driver.File.Routing (routing, Routes(..))
import Driver.File.Search (isSearchQuery, filterByQuery, searchPath)
import EffectTypes (FileAppEff(), FileComponentEff())
import Halogen (Driver())
import Halogen.HTML.Events.Monad (runEvent)
import Input.File (Input(), FileInput(..))
import Input.File.Item (ItemInput(..))
import Model.File (_items, _search, _path, _breadcrumbs, _sort, _salt, _isMount)
import Model.File.Breadcrumb (mkBreadcrumbs)
import Model.File.Item (Item(..))
import Model.File.Salt (Salt(..), newSalt)
import Model.File.Search (_loading, _value, _valid)
import Model.File.Sort (Sort(Asc))
import Model.Path (AnyPath(), DirPath(), hidePath)
import Model.Resource (Resource(..), isDirectory, isDatabase, root)
import Optic.Core ((..), (^.), (.~), (%~), (<>~))
import Optic.Refractor.Lens (_1, _2)
import Routing (matchesAff)
import Text.SlamSearch.Printer (strQuery)
import Text.SlamSearch.Types (SearchQuery())
import Utils (replaceLocation)

import qualified Data.Map as M

outside :: forall e. Driver Input (FileComponentEff e)
                  -> Eff (FileAppEff e) Unit
outside driver = handleRoute driver

handleRoute :: forall e. Driver Input (FileComponentEff e)
                      -> Eff (FileAppEff e) Unit
handleRoute driver = launchAff $ do
  -- TODO: after we fix black holes - search.timeout should be cleared when the URL changes
  var <- makeVar' initialAVar
  Tuple mbOld new <- matchesAff routing
  case new of
    Index -> liftEff $ updateURL Nothing Asc Nothing rootDir
    Sort sort -> liftEff $ updateURL Nothing sort Nothing rootDir
    SortAndQ sort query ->
      let queryParts = splitQuery query
      in liftEff $ updateURL queryParts.query sort Nothing queryParts.path
    Salted sort query salt -> do
      let newPage = maybe true id $ do
            old <- mbOld
            Tuple oldQuery oldSalt <- case old of
              Salted _ oldQuery oldSalt -> pure $ Tuple oldQuery oldSalt
              _ -> Nothing
            pure $ oldQuery /= query || oldSalt == salt
      Tuple c _ <- takeVar var
      cancel c $ error "cancel search"
      putVar var initialAVar
      if newPage
        then do
          let queryParts = splitQuery query
          liftEff $ driver $ inj $ WithState $ (_items .~ [])
                                            .. (_path .~ queryParts.path)
                                            .. (_breadcrumbs .~ mkBreadcrumbs queryParts.path)
                                            .. (_sort .~ sort)
                                            .. (_salt .~ salt)
                                            .. (_isMount .~ false)
                                            .. (_search .. _loading .~ true)
                                            .. (_search .. _value .~ maybe (This "") That queryParts.query)
                                            .. (_search .. _valid .~ true)
          listPath driver query 0 var (Directory queryParts.path)
          maybe (checkMount driver queryParts.path) (const $ pure unit) queryParts.query
        else
          liftEff $ driver $ inj $ WithState (_search .. _loading .~ false)

initialAVar :: Tuple (Canceler _) (M.Map Number Number)
initialAVar = Tuple mempty M.empty

updateURL :: forall e. Maybe String -> Sort -> Maybe Salt -> DirPath -> Eff _ Unit
updateURL query sort salt path = do
  salt' <- case salt of
    Nothing -> newSalt
    Just s -> pure s
  replaceLocation $ browseURL query sort salt' path

-- | Extracts the path and query value components from a SearchQuery value.
splitQuery :: SearchQuery -> { path :: DirPath, query :: Maybe String }
splitQuery q =
  let path = rootDir </> maybe currentDir id (searchPath q >>= parseAbsDir >>= sandbox rootDir)
  in { path: path
     , query: if isSearchQuery q
              then Just $ hidePath (renderPath $ Right path) (strQuery q)
              else Nothing
     }

listPath :: forall e. Driver Input (FileComponentEff e)
                   -> SearchQuery
                   -> Number
                   -> AVar (Tuple (Canceler _) (M.Map Number Number))
                   -> Resource
                   -> Aff _ Unit
listPath driver query deep var res = do
  modifyVar
    (_2 %~ M.alter (maybe (Just 1) (\x -> Just (x + 1))) deep) var

  canceler <- forkAff do
    ei <- attempt $ children res
    case ei of
      Left err -> liftEff $ runEvent (const $ pure unit) driver $ showError ("There was a problem listing the current directory: " ++ message err)
      Right ress -> do
        let next = filter (\x -> isDirectory x || isDatabase x) ress
            toAdd = filter (filterByQuery query) ress

        traverse_ (liftEff <<< driver <<< inj <<< ItemAdd) (Item <$> toAdd)

        if isSearchQuery query
          then traverse_ (listPath driver query (deep + 1) var) next
          else pure unit

    modifyVar
      (_2 %~ M.update (\v -> if v > 1 then Just (v - 1)
                             else Nothing) deep) var

    Tuple c r <- takeVar var
    if (foldl (+) 0 $ M.values r) == 0 then do
      liftEff do
        driver $ inj $ WithState (_search .. _loading .~ false)
      putVar var initialAVar
      else
      putVar var (Tuple c r)
  modifyVar (_1 <>~ canceler) var

checkMount :: forall e. Driver Input (FileComponentEff e)
                     -> DirPath
                     -> Aff _ Unit
checkMount driver path = do
  result <- attempt $ mountInfo (Database path)
  case result of
    Left _ -> pure unit
    Right _ -> liftEff $ driver $ inj $ WithState (_isMount .~ true)
