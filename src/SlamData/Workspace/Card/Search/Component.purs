{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Card.Search.Component
  ( searchComponent
  , module SlamData.Workspace.Card.Search.Component.Query
  , module SlamData.Workspace.Card.Search.Component.State
  ) where

import SlamData.Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Lens ((.~))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Render.Common as RC
import SlamData.Render.CSS as CSS
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as NC
--import SlamData.Workspace.Card.Eval as Eval
import SlamData.Workspace.Card.Search.Component.Query (Query, SearchQuery(..))
import SlamData.Workspace.Card.Search.Component.State (State, _running, _searchString, initialState)

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

searchComponent ∷ H.Component NC.CardStateP NC.CardQueryP Slam
searchComponent =
  NC.makeCardComponent
    { cardType: CT.Search
    , component: H.component { render, eval }
    , initialState: initialState
    , _State: NC._SearchState
    , _Query: NC.makeQueryPrism NC._SearchQuery
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.form, HH.className "sd-search-field" ] ]
    [ HH.input
        [ HP.inputType HP.InputText
        , HP.placeholder "Input search string"
        , HE.onValueInput $ HE.input \str → UpdateSearch str ⋙ right
        , HP.value state.searchString
        ]
    , HH.button
        [ HP.class_ (HH.className "sd-search-state-btn")
        , HE.onClick $ HE.input_ (UpdateSearch "" ⋙ right)
        ]
        [ HH.img [ HP.src $ if state.running then "img/spin.gif" else "img/remove.svg" ] ]
    , HH.button
        [ HE.onClick $ HE.input_ (NC.NotifyRunCard ⋙ left) ]
        [ RC.glyph B.glyphiconSearch ]
    ]

eval ∷ Natural Query DSL
eval = coproduct cardEval searchEval

cardEval ∷ Natural NC.CardEvalQuery DSL
cardEval q =
  case q of
    NC.EvalCard input output next →
      pure next
    NC.NotifyRunCard next →
      pure next
    NC.NotifyStopCard next →
      pure next
    NC.Save k → do
      input ← H.gets _.searchString
      pure $ k $ encodeJson input
    NC.Load json next → do
      for_ (decodeJson json) \input →
        H.modify $ _searchString .~ input
      pure next
    NC.SetCanceler _ next → pure next
    NC.SetDimensions _ next → pure next

searchEval ∷ Natural SearchQuery DSL
searchEval (UpdateSearch str next) =
  H.modify (_searchString .~ str) $> next
