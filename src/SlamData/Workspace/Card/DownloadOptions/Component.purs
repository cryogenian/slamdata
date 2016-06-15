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

module SlamData.Workspace.Card.DownloadOptions.Component where

import SlamData.Prelude

import Data.Lens as Lens
import Data.Lens ((.~), (^?), _Left, _Right, (%~))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Common.EvalQuery as Ec
import SlamData.Workspace.Card.Component as Cc
import SlamData.Workspace.Card.Component (makeCardComponent, makeQueryPrism, _DownloadOptionsState, _DownloadOptionsQuery)
import SlamData.Workspace.Card.DownloadOptions.Component.Query (QueryP, Query(..))
import SlamData.Workspace.Card.DownloadOptions.Component.State (State, _compress, _options, _source, initialState, _levelOfDetails)
import SlamData.Render.CSS as Rc
import SlamData.Effects (Slam)
import SlamData.Download.Model as D
import SlamData.Download.Render as Rd
import SlamData.Workspace.Card.CardType (CardType(DownloadOptions))
import SlamData.Workspace.Card.Port as P
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Render.Common (glyph)

type HTML = H.ComponentHTML QueryP
type DSL = H.ComponentDSL State QueryP Slam

comp ∷ Cc.CardComponent
comp = makeCardComponent
  { cardType: DownloadOptions
  , component: H.component {render, eval}
  , initialState: initialState
  , _State: _DownloadOptionsState
  , _Query: makeQueryPrism _DownloadOptionsQuery
  }

render ∷ State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD state
    ]

renderHighLOD ∷ State → HTML
renderHighLOD state =
  case state.source of
    Nothing →
      HH.div
        [ HP.classes
            $ [ B.alert, B.alertDanger ]
            ⊕ hideClasses
        ]
        [ HH.text "The current input cannot be downloaded" ]
    Just _ →
      HH.div
        [ HP.classes
            $ [ Rc.downloadCardEditor
              , HH.className "card-input-maximum-lod"
              ]
            ⊕ hideClasses
        ]
        [ renderDownloadTypeSelector state
        , renderDownloadConfiguration state
        ]
  where
  hideClasses = guard (state.levelOfDetails ≠ High) $> B.hidden

renderLowLOD ∷ State → HTML
renderLowLOD state =
  HH.div
    [ HP.classes
        $ [ HH.className "card-input-minimum-lod" ]
        ⊕ hideClasses
    ]
    [ HH.button
      [ ARIA.label "Expand to see download options"
      , HP.title "Expand to see download options"
      , HP.disabled true
      ]
      [ glyph B.glyphiconDownloadAlt
      , HH.text "Please, expand to see options"
      ]
    ]

  where
  hideClasses = guard (state.levelOfDetails ≠ Low) $> B.hidden


renderDownloadConfiguration ∷ State → HTML
renderDownloadConfiguration state =
  HH.div
    [ HP.classes [ Rc.downloadConfiguration ] ]
    $ [ either optionsCSV optionsJSON state.options ]
    ⊕ [ compress state ]


optionsCSV ∷ D.CSVOptions → HTML
optionsCSV = Rd.optionsCSV (\lens v → right ∘ (ModifyCSVOpts (lens .~ v)))

optionsJSON ∷ D.JSONOptions → HTML
optionsJSON = Rd.optionsJSON (\lens v → right ∘ (ModifyJSONOpts (lens .~ v)))

compress ∷ State → HTML
compress state =
  HH.div
    [ HP.classes [ B.formGroup ] ]
    [ HH.label_
        [ HH.span_ [ HH.text "Compress" ]
        , HH.input
            [ HP.inputType HP.InputCheckbox
            , HP.checked $ compressed state
            , HP.enabled $ isJust state.source
            , HE.onValueChange (HE.input_ (right ∘ ToggleCompress))
            ]
        ]
    ]
  where
  compressed ∷ State → Boolean
  compressed state = isJust state.source || state.compress

renderDownloadTypeSelector ∷ State → HTML
renderDownloadTypeSelector state =
  HH.div
    [ HP.classes [ Rc.downloadTypeSelector ] ]
    [ HH.img
        [ HP.src "img/csv.svg"
        , HP.classes (guard (isLeft state.options) $> B.active)
        , HP.title "Comma separated values"
        , HE.onClick (HE.input_ (right ∘ SetOutput D.CSV))
        ]
    , HH.img
        [ HP.src "img/json.svg"
        , HP.classes (guard (isRight state.options) $> B.active)
        , HP.title "JSON"
        , HE.onClick (HE.input_ (right ∘ SetOutput D.JSON))
        ]
    ]

eval ∷ QueryP ~> DSL
eval = coproduct cardEval downloadOptsEval

cardEval ∷ Ec.CardEvalQuery ~> DSL
cardEval (Ec.EvalCard info output next) = do
  H.modify $ _source .~ info.input ^? Lens._Just ∘ P._Resource
  pure next
cardEval (Ec.Save k) = map (k ∘ Card.DownloadOptions) H.get
cardEval (Ec.Load card next) = do
  case card of
    Card.DownloadOptions st → H.set st
    _ → pure unit
  pure next
cardEval (Ec.SetDimensions dims next) = do
  H.modify
    $ _levelOfDetails
    .~ if dims.width < 648.0 ∨ dims.height < 240.0
         then Low
         else High
  pure next

downloadOptsEval ∷ Query ~> DSL
downloadOptsEval (SetOutput ty next) = do
  options ← H.gets _.options
  case ty, options of
    D.CSV, (Right _) → H.modify _{options = Left D.initialCSVOptions}
    D.JSON, (Left _) → H.modify _{options = Right D.initialJSONOptions}
    _, _ → pure unit
  pure next
downloadOptsEval (ModifyCSVOpts fn next) =
 H.modify (_options ∘ _Left %~ fn) $> next
downloadOptsEval (ModifyJSONOpts fn next) =
  H.modify (_options ∘ _Right %~ fn) $> next
downloadOptsEval (ToggleCompress next) = H.modify (_compress %~ not) $> next
