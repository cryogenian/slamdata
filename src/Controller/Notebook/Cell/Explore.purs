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

module Controller.Notebook.Cell.Explore (runExplore, viewExplore) where

import Prelude
import Control.Plus (empty)
import Control.Monad.Eff.Class (liftEff)
import Controller.Notebook.Cell.JTableContent (runJTable)
import Controller.Notebook.Common (I())
import Data.Date (now)
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), fromMaybe)
import Input.Notebook (Input(CellResult))
import Model.Notebook.Port (_PortResource, _PortInvalid)
import Model.Notebook.Cell (Cell(), _FileInput, _content, _cellId, _input)
import Model.Notebook.Cell.FileInput (_file)
import Optic.Core
import Optic.Fold ((^?))
import Optic.Refractor.Prism (_Just)

import qualified Data.Array.NonEmpty as NEL
import qualified Data.Int as I

runExplore :: forall e. Cell -> I e
runExplore cell = case cell ^? _input .. _PortResource of
  Just file -> runJTable file cell
  Nothing -> do
    now' <- liftEff now
    let msg = fromMaybe "Please select a file" (cell ^? _input .. _PortInvalid)
    return $ inj $ CellResult (cell ^. _cellId) now' (Left $ NEL.singleton msg)


viewExplore :: forall e. Cell -> I e
viewExplore cell =
  fromMaybe empty ((flip runJTable cell) <$> (cell ^? _input.._PortResource))
