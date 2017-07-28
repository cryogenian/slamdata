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

module SlamData.Workspace.Dialog.VizUnavailable.Component
  ( State
  , Query(..)
  , Message(..)
  , component
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Record as R
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Card.Setups.DimensionMap.Package as DP
import SlamData.Workspace.Card.Setups.DimensionMap.DSL as DSL
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Axis as Ax

type State =
  { vizType ∷ VT.VizType
  , axes ∷ Ax.Axes
  }

data Query a = Raise Message a

data Message = Dismiss

component ∷ ∀ m. H.Component HH.HTML Query State Message m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }


-- Providing better texts would be helpful
render ∷ State → H.ComponentHTML Query
render state =
  HH.div
  [ HP.classes [ HH.ClassName "deck-dialog-embed" ] ]
  [ HH.h4_
      [ HH.text $ VT.name state.vizType ⊕ " is unavailable for this data set"
      ]
  , HH.div
      [ HP.classes [ HH.ClassName "deck-dialog-body" ] ]
      [ HH.p_
        [ HH.text $ "To be visualizable data set must have at least" ]
      , HH.ul_ $ A.fold
          [ mustHaveBut _.value "measure" ""
          , mustHaveBut _.category "categorical" ""
          , mustHaveBut _.date "date" ""
          , mustHaveBut _.datetime "datetime" ""
          , mustHaveBut _.time "time" ""
          , mustHaveBut _.nonMeasure "non-measure" ""
          , mustHaveBut _.total "" "in total"
          ]
      ]
  , HH.div
      [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
      [ HH.button
          [ HP.classes [ CN.btn, CN.btnDefault ]
          , HE.onClick (HE.input_ (Raise Dismiss))
          ]
          [ HH.text "Dismiss" ]
      ]
  ]
  where
  reqs = maybe DSL.noneRequirements _.axesRequirements $ DP.getPackage state.vizType
  sizes =
    let
      rs = Ax.axesSizes state.axes
      nonMeasure = rs.category + rs.date + rs.time + rs.datetime
      total = nonMeasure + rs.value
    in
     rs
       # R.insert (SProxy ∷ SProxy "nonMeasure") nonMeasure
       # R.insert (SProxy ∷ SProxy "total") total

  mustHaveBut getter prefix suffix = do
    guard (getter reqs > getter sizes)
    let have = case getter sizes of
          0 → ", but have no any"
          n → ", but have only " ⊕ show n
    pure $ HH.li_
      [ HH.text
        $ ( show $ getter reqs )
        ⊕ " " ⊕ prefix ⊕ " "
        ⊕ "axes "
        ⊕ suffix
        ⊕ have
      ]


eval ∷ ∀ m. Query ~> H.ComponentDSL State Query Message m
eval (Raise msg next) = H.raise msg $> next
