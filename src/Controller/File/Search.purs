module Controller.File.Search where

import Control.Inject1 (inj)
import Control.Monad.Aff (makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomInt)
import Control.Timer (timeout, clearTimeout)
import Controller.File.Common (toInput)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Driver.File (updateQ, updateSalt)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (Event(), runEvent, async)
import Input.File (Input(), FileInput(Loading))
import Input.File.Search (SearchInput(..))
import Model.Search (Search())
import Routing.Hash (modifyHash)
import Text.SlamSearch (mkQuery)

handleSearchClear :: forall e. Boolean -> Search -> Event (FileAppEff e) Input
handleSearchClear isSearching search = do
  liftEff $ maybe (pure unit) clearTimeout search.timeout
  if isSearching then do
    rnd <- show <$> (liftEff $ randomInt 1000000 2000000)
    liftEff (modifyHash $ updateSalt rnd)
    toInput $ Loading false
    else
    setQE "path:/"

-- TODO: String/Path type alias? Maybe a real path when new library is done
handleSearchChange :: forall e. Search -> String -> String -> Event (FileAppEff e) Input
handleSearchChange search ch path = async $ makeAff $ \_ k -> do
    k $ inj $ SearchNextValue ch
    maybe (pure unit) clearTimeout search.timeout
    tim <- timeout Config.searchTimeout $ do
      runEvent (const $ pure unit) (const $ pure unit) $
        setQE (ch <> " path:\"" <> path <> "\"")
    k $ inj $ SearchTimeout tim
    k $ inj $ SearchValidation true
  -- ATTENTION
  -- This works too slow
  --      (toInput $ M.SearchNextValue ch) `andThen` \_ -> do
  --        tim <- liftEff $ timeout Config.searchTimeout $ do
  --          runEvent (const $ pure unit) (const $ pure unit) $
  --            setQE (ch <> " path:\"" <> p <> "\"")
  --        (toInput $ M.SearchTimeout tim) `andThen` \_ -> do
  --          toInput $ M.SearchValidation true

-- TODO: String/Path type again?
handleSearchSubmit :: forall e. Search -> String -> Event (FileAppEff e) Input
handleSearchSubmit search path = do
  liftEff $ maybe (pure unit) clearTimeout search.timeout
  setQE (search.nextValue <> " +path:" <> path)

setQE :: forall e. String -> Event (FileAppEff e) Input
setQE q = do
  case mkQuery q of
    Left _ | q /= "" -> toInput $ SearchValidation false
    Right _ -> do
      liftEff (modifyHash $ updateQ q)
      toInput $ SearchValidation true
    _ -> do
      liftEff (modifyHash $ updateQ "")
      toInput $ SearchValidation true
