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

module Driver.Notebook.Notify where

import Prelude
import qualified Config as Config
import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (Ref(), readRef, modifyRef, writeRef)
import Control.Timer (Timeout(), timeout, clearTimeout)
import Controller.Notebook.Cell (requestCellContent)
import Controller.Notebook.Common (I())
import Data.Array (filter, elemIndex, (!!), singleton, head)
import Data.Either (Either(..))
import Data.Foldable (traverse_, fold, elem)
import Data.Map (toList, empty, insert, lookup, Map(), delete)
import Data.Maybe (maybe, Maybe(..), isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe.First (First(..), runFirst)
import Data.Monoid.Conj (Conj(..), runConj)
import Data.Tuple (fst, snd)
import EffectTypes
import Halogen (Driver())
import Input.Notebook (Input(..))
import Model.Notebook (State(), _notebook, _requesting, _refreshing)
import Model.Notebook.Cell (Cell(), _cellId, CellId(), _hasRun)
import Model.Notebook.Domain
import Optic.Core
import Optic.Fold ((^?))


type NotifyKnot = Map CellId Timeout

notifyDriver :: forall e. Ref State -> Ref NotifyKnot -> Input ->
                Driver Input (NotebookComponentEff e) ->
                Eff (NotebookAppEff e) Unit
notifyDriver sKnot nKnot input driver =
  case input of
    -- We've got result from other cell, just notify children
    CellResult cellId _ (Right _) -> do
      state <- readRef sKnot
      go state cellId
    -- Markdown cell's updated, check timeout and notify children
    CellSlamDownEvent cellId _ -> do
      state <- readRef sKnot
      map <- readRef nKnot
      maybe
        (setTimeout state cellId)
        (\t -> do
            clearTimeout t
            setTimeout state cellId)
        (lookup cellId map)
    RefreshCell cell -> do
      state <- readRef sKnot
      let ancs = ancestors (cell ^. _cellId) (state ^. _notebook .. _dependencies)
      maybe (driver $ RequestCellContent cell) (goHead state) $ head ancs
    _ -> pure unit
  where
  goHead :: _ -> CellId -> Eff _ Unit
  goHead state cid =
    maybe (pure unit) (driver <<< RequestCellContent) $
    state ^? _notebook .. cellById cid
  setTimeout :: State -> CellId -> Eff _ Unit
  setTimeout state cellId = do
    t <- timeout Config.notifyTimeout do
      go state cellId
      modifyRef nKnot (delete cellId)
    modifyRef nKnot (insert cellId t)

  go :: State -> CellId -> Eff _ Unit
  go state cellId = do
    notify (state ^. _notebook) cellId (state ^. _requesting) (state ^. _refreshing) driver
    cleanId cellId

  cleanId :: CellId -> Eff _ Unit
  cleanId cellId =
    driver $ WithState ((_requesting %~ filter (/= cellId))
                     .. (_refreshing %~ filter (/= cellId)))

notify :: forall e. Notebook -> CellId -> Array CellId -> Array CellId ->
          Driver Input (NotebookComponentEff e) ->
          Eff (NotebookAppEff e) Unit
notify notebook cellId requestedIds refreshingIds driver = do
  -- If current cell is parent for all requested or refreshing cells
  -- and we can get next cell in ancestors list
  if isParent && isJust nextCell
  -- then we get this next cell
    then request $ fromJust nextCell
  -- else we get all children
    else requestForChildren
  where
  request :: Cell -> Eff _ Unit
  request = driver <<< RequestCellContent

  requestForChildren :: Eff _ Unit
  requestForChildren =
    if elem cellId refreshingIds
    then pure unit
    else traverse_ request $ dependentCells cellId

  -- list of list of dependencies of requested cell from top to bottom
  ancestors' :: Array (Array CellId)
  ancestors' = flip ancestors (notebook ^._dependencies) <$>
               (requestedIds <> refreshingIds)

  -- if current cell is parent for all requested cells
  isParent :: Boolean
  isParent = runConj $ fold (Conj <$> (elem cellId) <$> ancestors')

  -- if current cell is parent for all requested cells we get
  -- next cell in this hierarchy and return it
  nextCell :: Maybe Cell
  nextCell = do
    cid <- runFirst $ fold
           (First <$> ((\as -> do
                           i <- elemIndex cellId as
                           as !! (i + one)) <$> ancestors'))

    notebook ^? cellById cid

  dependentCells :: CellId -> Array Cell
  dependentCells cid = filter (\x -> elem (x ^._cellId) (dependencies cid) && (x ^. _hasRun))
                       (notebook ^. _cells)

  dependencies :: CellId -> Array CellId
  dependencies = flip descendants (notebook ^._dependencies)

