{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.CardType.VizType where

import SlamData.Prelude

import SlamData.Workspace.Card.CardType.Chart as Cht
import SlamData.Workspace.Card.CardType.Geo as Geo
import SlamData.Workspace.Card.CardType.Input as Inp
import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.CardType.Static as Sta

type VizType =
  Variant ( Cht.ChartR ( Geo.GeoR ( Sel.SelectR ( Inp.InputR ( Sta.StaticR () ) ) ) ) )

all ∷ Array VizType
all = Cht.all ⊕ Geo.all ⊕ Inp.all ⊕ Sel.all ⊕ Sta.all
