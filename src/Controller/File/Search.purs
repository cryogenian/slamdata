module Controller.File.Search where

import Prelude
import Control.Monad.Aff (makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Control.Timer (timeout, clearTimeout)
import Controller.File.Common (Event(), toInput, browseURL)
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.These (theseLeft, theseRight, thisOrBoth)
import Data.Maybe (Maybe(..), maybe)
import Halogen.HTML.Events.Monad (runEvent, async, andThen)
import Input.File (FileInput(..))
import Model.File (State(), _sort, _path, _search)
import Model.File.Salt (newSalt)
import Model.File.Search (_loading, _value, _timeout, _valid)
import Optic.Core 
import Text.SlamSearch (mkQuery)
import Utils (setLocation)

handleSearchClear :: forall e. State -> Event e
handleSearchClear state = do
  salt <- liftEff newSalt
  liftEff $ setLocation $ browseURL Nothing (state ^. _sort) salt (state ^. _path)
  empty

handleSearchChange :: forall e. State -> String -> Event e
handleSearchChange state value = do
  liftEff $ maybe (pure unit) clearTimeout (state ^. _search .. _timeout)
  let updateValue = _search .. _value %~ (thisOrBoth value <<< theseRight)
  (toInput $ WithState updateValue)
    `andThen` \_ -> async $ makeAff \_ k -> do
      tim <- timeout Config.searchTimeout $
        runEvent (const $ pure unit) k $
          handleSearchSubmit (updateValue state)
      k $ inj $ WithState (_search .. _timeout ?~ tim)

handleSearchSubmit :: forall e. State -> Event e
handleSearchSubmit state = do
  salt <- liftEff newSalt
  case theseLeft (state ^. _search .. _value) of
    Just q -> case mkQuery q of
      Left _ | q /= "" -> toInput $ WithState (_search .. _valid .~ false)
      _ -> do
        liftEff $ setLocation $ browseURL (Just q) (state ^. _sort) salt (state ^. _path)
        empty
    _ -> empty
