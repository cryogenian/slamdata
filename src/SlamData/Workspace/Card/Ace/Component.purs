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

module SlamData.Workspace.Card.Ace.Component
  ( aceComponent
  , DSL
  , HTML
  , AceEval
  , module SlamData.Workspace.Card.Ace.Component.Query
  , module SlamData.Workspace.Card.Ace.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (liftEff)

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceQuery(..), AceState, Autocomplete(..), aceConstructor)
import Ace.Types (Editor)

import Data.Lens ((.~))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.Ace.Component.Query (QueryP)
import SlamData.Workspace.Card.Ace.Component.State (State, StateP, initialState, _levelOfDetails)
import SlamData.Workspace.Card.Ace.Model as Model
import SlamData.Workspace.Card.CardType (CardType(Ace), AceMode, aceMode, aceCardGlyph)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery(..), CardEvalInput)
import SlamData.Workspace.Card.Component (CardStateP, CardQueryP, makeCardComponent, makeQueryPrism, _AceState, _AceQuery)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Effects (Slam)
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as CSS

import Utils.Ace (getRangeRecs, readOnly)

type DSL = H.ParentDSL State AceState CardEvalQuery AceQuery Slam Unit
type HTML = H.ParentHTML AceState CardEvalQuery AceQuery Slam Unit
type AceEval = CardEvalInput → DSL Unit

type AceConfig =
  { mode ∷ AceMode
  , eval ∷ AceEval
  }

aceComponent ∷ AceConfig → H.Component CardStateP CardQueryP Slam
aceComponent cfg = makeCardComponent
  { cardType: Ace cfg.mode
  , component: H.parentComponent { render: render cfg, eval, peek: Nothing }
  , initialState: H.parentState initialState
  , _State: _AceState
  , _Query: makeQueryPrism _AceQuery
  }

  where

  eval ∷ CardEvalQuery ~> DSL
  eval (NotifyRunCard next) = pure next
  eval (NotifyStopCard next) = pure next
  eval (EvalCard info output next) = do
    -- TODO: check!
    cfg.eval info
    pure next
    --content ← fromMaybe "" <$> H.query unit (H.request GetText)
    --result ← evaluator info content
    --pure $ k result
  eval (Save k) = do
    content ← fromMaybe "" <$> H.query unit (H.request GetText)
    mbEditor ← H.query unit (H.request GetEditor)
    rrs ← H.fromEff $ maybe (pure []) getRangeRecs $ join mbEditor
    pure $ k $ Model.encode { text: content, ranges: rrs }
  eval (Load json next) = do
    let model = either (const Model.emptyModel) id $ Model.decode json
        text = model.text
        ranges = model.ranges
    H.query unit $ H.action (SetText text)
    mbEditor ← H.query unit $ H.request GetEditor
    H.fromEff $ for_ (join mbEditor) \editor → do
      traverse_ (readOnly editor) ranges
      Editor.navigateFileEnd editor
    pure next
  eval (SetCanceler _ next) = pure next
  eval (SetDimensions dims next) = do
    H.modify
      $ _levelOfDetails
      .~ if dims.width < 240.0 then Low else High
    pure next


aceSetup ∷ AceConfig → Editor → Slam Unit
aceSetup cfg editor = liftEff do
  Editor.setTheme "ace/theme/chrome" editor
  Editor.setEnableLiveAutocompletion true editor
  Editor.setEnableBasicAutocompletion true editor
  Session.setMode (aceMode cfg.mode) =<< Editor.getSession editor




render ∷ AceConfig → State → HTML
render cfg state =
  HH.div_
    [ renderHighLOD cfg state
    , renderLowLOD cfg state
    ]

renderHighLOD ∷ AceConfig → State → HTML
renderHighLOD cfg state =
  HH.div
  [ HP.classes
      $ [ CSS.cardInput, CSS.aceContainer ]
      ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
  ]
  [ HH.Slot (aceConstructor unit (aceSetup cfg) (Just Live) ) ]

renderLowLOD ∷ AceConfig → State → HTML
renderLowLOD cfg state =
  HH.div
    [ HP.classes
        $ (guard (state.levelOfDetails ≠ Low) $> B.hidden)
        ⊕ [ HH.className "card-input-minimum-lod" ]
    ]
    [ HH.button
        [ ARIA.label "Expand to edit"
        , HP.title "Expand to edit"
        , HP.disabled true
        ]
        [ glyph $ aceCardGlyph cfg.mode
        , HH.text "Please, expand to edit"
        ]
    ]
