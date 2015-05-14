module Controller.File.Search where

import Data.Inject1 (inj)
import Control.Monad.Aff (makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomInt)
import Control.Timer (timeout, clearTimeout)
import Controller.File.Common (toInput)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Driver.File.Path (updateQ, updateSalt)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (Event(), runEvent, async)
import Input.File (Input(), FileInput(Loading))
import Input.File.Search (SearchInput(..))
import Model.File.Search (Search())
import Routing.Hash (modifyHash)
import Text.SlamSearch (mkQuery)
import Model.Resource (DirPath())
import Data.Path.Pathy

handleSearchClear :: forall e. Boolean -> Search -> Event (FileAppEff e) Input
handleSearchClear isSearching search = do
  liftEff $ maybe (pure unit) clearTimeout search.timeout
  if isSearching then do
    rnd <- show <$> (liftEff $ randomInt 1000000 2000000)
    liftEff (modifyHash $ updateSalt rnd)
    toInput $ Loading false
    else
    setQE "path:/"

handleSearchChange :: forall e. Search -> String -> DirPath -> Event (FileAppEff e) Input
handleSearchChange search ch path = async $ makeAff $ \_ k -> do
    k $ inj $ SearchSet ch
    maybe (pure unit) clearTimeout search.timeout
    tim <- timeout Config.searchTimeout $ do
      runEvent (const $ pure unit) k $
        setQE (ch <> " path:\"" <> printPath path <> "\"")
    k $ inj $ SearchTimeout tim
    k $ inj $ SearchValidation true

handleSearchSubmit :: forall e. Search -> DirPath -> Event (FileAppEff e) Input
handleSearchSubmit search path = do
  liftEff $ maybe (pure unit) clearTimeout search.timeout
  setQE (search.value <> " +path:" <> printPath path)

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
