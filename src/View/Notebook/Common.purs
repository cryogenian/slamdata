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

module View.Notebook.Common where

import Prelude
import Controller.Notebook (I())
import EffectTypes (NotebookAppEff())
import Halogen.HTML.Attributes (Attr(), attr, attributeName)
import Halogen.HTML.Events.Monad (Event())
import Input.Notebook (Input())
import Model.Notebook.Cell (CellContent(), cellContentType, CellId())
import qualified Halogen.HTML as H

type HTML e = H.HTML (I e)

dataCellId :: forall i. CellId -> Attr i
dataCellId = attr (attributeName "data-cell-id") <<< show

dataCellType :: forall i. CellContent -> Attr i
dataCellType = attr (attributeName "data-cell-type") <<< cellContentType

dataEChartsId :: forall i. String -> Attr i
dataEChartsId = attr (attributeName "data-echarts-id")
