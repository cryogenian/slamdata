-- | Module handles outer messages to `halogen` application
-- | Mostly consists of routing functions
module Driver.File
  ( outside ) where

import Data.Inject1 (inj)
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random
import Control.Monad.Aff (launchAff, cancel, attempt, Canceler(), Aff(), forkAff)
import Control.Monad.Aff.AVar (makeVar', takeVar, putVar, modifyVar, AVar(), AVAR())
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Tuple
import Data.Array (filter)
import Data.Monoid (mempty)
import qualified Data.Map as M
import Optic.Core
import Optic.Refractor.Lens

import EffectTypes (FileAppEff(), FileComponentEff())
import Input.File (Input(), FileInput(..))
import Input.File.Item (ItemInput(..))
import Input.File.Search (SearchInput(..))

import qualified Halogen as Hl
import qualified Text.SlamSearch.Printer as S
import Model.Sort (Sort(Asc))
import Model.Path (cleanPath, hidePath)
import Model.File.Item (wrap)
import Api.Fs (children)
import Routing (matchesAff)
import Routing.Hash.Aff (setHash, modifyHash)
import Text.SlamSearch.Types (SearchQuery())
import Text.SlamSearch.Printer (strQuery)

import Driver.File.Routing (routing, Routes(..))
import Driver.File.Search (isSearchQuery, filterByQuery, searchPath)
import Driver.File.Path (updateSalt, updatePath, setSort, renderPath)
import Model.Resource
import Data.Path.Pathy (
  currentDir,
  parseAbsDir,
  sandbox,
  printPath,
  Path(), Sandboxed(), Rel(),
  relativeTo,
  rootDir,
  (</>))

outside :: forall e. Hl.Driver Input (FileComponentEff e) ->
           Eff (FileAppEff e) Unit
outside driver = handleRoute driver


initialAVar :: Tuple (Canceler _) (M.Map Number Number)
initialAVar = Tuple mempty M.empty

handleRoute :: forall e.
               Hl.Driver Input (FileComponentEff e) ->
               Eff (FileAppEff e) Unit
handleRoute driver = launchAff $ do
  var <- makeVar' initialAVar
  Tuple mbOld new <- matchesAff routing
  case new of
    Index -> do
      setHash $ setSort Asc
    Sort sort -> do
      modifyHash $ updatePath (getPath root)
    SortAndQ sort query -> do
      rnd <- show <$> (liftEff $ randomInt 1000000 2000000)
      modifyHash $ updateSalt rnd

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
      liftEff (driver $ inj $ Loading false)
      if newPage then do
        let path :: forall a b. DirPath
            path = rootDir </>
                   (maybe currentDir id $
                    (searchPath query >>= parseAbsDir >>= sandbox rootDir))
        liftEff $ do
          driver $ inj $ Loading true
          driver $ inj $ SetPath path 
          driver $ inj $ SearchSet $ (hidePath (renderPath $ inj path) $ (strQuery query))
          driver $ inj $ ItemsUpdate [] sort
          driver $ inj $ SetSearching (isSearchQuery query)
        listPath driver query 0 var (newDirectory # pathL .~ Right path)
        else do
        pure unit

listPath :: forall e. Hl.Driver Input (FileComponentEff e) ->
            SearchQuery -> Number ->
            AVar (Tuple (Canceler _) (M.Map Number Number)) ->
            Resource -> Aff _ Unit
listPath driver query deep var res = do
  modifyVar
    (\t -> t # _2 %~ M.alter (maybe (Just 1) (\x -> Just (x + 1))) deep) var

  canceler <- forkAff do
    ei <- attempt $ children res
    flip (either (const $ pure unit)) ei \ress -> do
      let next = filter (\x -> isDirectory x || isDatabase x) ress
          toAdd = filter (filterByQuery query) ress

      traverse_ (liftEff <<< driver <<< inj <<< ItemAdd) (wrap <$> toAdd)

      if isSearchQuery query 
        then traverse_ (listPath driver query (deep + 1) var) next
        else pure unit

    modifyVar
      (\t -> t # _2 %~ M.update (\v -> if v > 1 then Just (v - 1)
                                       else Nothing) deep) var

    Tuple c r <- takeVar var
    if (foldl (+) 0 $ M.values r) == 0 then do
      liftEff do
        driver $ inj $ Loading false
      putVar var initialAVar
      else
      putVar var (Tuple c r)
  modifyVar (\t -> t # _1 %~ (<> canceler)) var
