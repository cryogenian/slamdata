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

module Controller.Notebook.Cell.Query where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.Notebook.Cell.JTableContent (queryToJTable, runJTable)
import Controller.Notebook.Common (I())
import Data.Date (now)
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), fromMaybe)
import Input.Notebook (Input(CellResult))
import Model.Notebook.Cell (Cell(), _Query, _content, _input, _output, _cellId, outFile)
import Model.Notebook.Port (_PortResource)
import Model.Resource (Resource(), parent, root)
import Optic.Core
import Optic.Fold ((^?))

import qualified Data.Array.NonEmpty as NEL
import qualified Model.Notebook.Cell.Query as Qu

runQuery :: forall e. Cell -> I e
runQuery cell = case queryToJTable cell input <$> path <*> (pure $ outFile cell) of
  Just x -> x
  Nothing -> do
    now' <- liftEff now
    return $ inj $ CellResult (cell ^. _cellId) now' (Left $ NEL.singleton "Cannot run query with no output resource")

  where
  output :: Maybe Resource
  output = cell ^? _output .. _PortResource

  path :: Maybe Resource
  path = parent <$> output

  input :: String
  input = cell ^. _content .. _Query .. Qu._input


viewQuery :: forall e. Cell -> I e
viewQuery cell =
  fromMaybe empty ((flip runJTable cell) <$> (cell ^? _output.._PortResource))
