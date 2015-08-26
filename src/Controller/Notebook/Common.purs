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

module Controller.Notebook.Common where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Data.Date (now, nowEpochMilliseconds, toEpochMilliseconds)
import Data.Maybe (Maybe(..), maybe)
import Data.Time (Milliseconds())
import EffectTypes (NotebookAppEff())
import Halogen.HTML.Events.Monad (Event(), andThen)
import Input.Notebook (Input(..))
import Model.Notebook.Cell (Cell(), RunState(..), _RunningSince, _cellId, _runState)
import Optic.Core
import Optic.Extended ((^?))

type I e = Event (NotebookAppEff e) Input

run :: forall e. Cell -> I e
run cell = StartRunCell (cell ^. _cellId) <$> liftEff now

update :: forall e. Cell -> (Cell -> Cell) -> I e
update cell = pure <<< UpdateCell (cell ^. _cellId)

finish :: forall e. Cell -> I e
finish cell = do
  d <- liftEff nowEpochMilliseconds
  update cell (_runState .~ RunFinished (maybe zero (d -) started))
  where
  started :: Maybe Milliseconds
  started = toEpochMilliseconds <$> (cell ^? _runState .. _RunningSince)
