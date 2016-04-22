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

module SlamData.Notebook.Card.Download.Component where

import SlamData.Prelude

import Data.Lens ((?~), (.~), (%~), _Left, _Right, preview)
import Data.Path.Pathy (printPath)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Quasar.Paths as Paths

import SlamData.Download.Model as D
import SlamData.Download.Render as Rd
import SlamData.Effects (Slam)
import SlamData.Notebook.Card.CardType (CardType(Download))
import SlamData.Notebook.Card.Common.EvalQuery as Ec
import SlamData.Notebook.Card.Component (makeCardComponent, makeQueryPrism, _DownloadState, _DownloadQuery)
import SlamData.Notebook.Card.Component as Cc
import SlamData.Notebook.Card.Download.Component.Query (QueryP, Query(..))
import SlamData.Notebook.Card.Download.Component.State (State, _compress, _options, _source, decode, encode, initialState)
import SlamData.Notebook.Card.Port as P
import SlamData.Quasar (reqHeadersToJSON, encodeURI)
import SlamData.Render.Common (row)
import SlamData.Render.CSS as Rc

import Utils.Path as PU

type DownloadHTML = H.ComponentHTML QueryP
type DownloadDSL = H.ComponentDSL State QueryP Slam

downloadComponent ∷ Cc.CardComponent
downloadComponent = makeCardComponent
  { cardType: Download
  , component: H.component { render, eval }
  , initialState: initialState
  , _State: _DownloadState
  , _Query: makeQueryPrism _DownloadQuery
  }

render ∷ State → DownloadHTML
render state =
  case state.source of
    Nothing →
      HH.div
        [ HP.classes [ B.alert, B.alertDanger ] ]
        [ HH.text "The current input cannot be downloaded" ]
    Just _ →
      HH.div
        [ HP.classes [ Rc.downloadCardEditor ] ]
        [ renderDownloadTypeSelector state
        , renderDownloadConfiguration state
        ]

renderDownloadTypeSelector ∷ State → DownloadHTML
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

renderDownloadConfiguration  ∷ State → DownloadHTML
renderDownloadConfiguration state =
  HH.div
    [ HP.classes [ Rc.downloadConfiguration ] ]
    $  [ either optionsCSV optionsJSON state.options]
    <> [ row
           [ HH.div [ HP.classes [ B.colXs4 ] ] [ compress state ]
           , HH.div [ HP.classes [ B.colXs8 ] ] [ downloadButton state ]
           ]
       ]

optionsCSV ∷ D.CSVOptions → DownloadHTML
optionsCSV = Rd.optionsCSV (\lens v → right ∘ (ModifyCSVOpts (lens .~ v)))

optionsJSON ∷ D.JSONOptions → DownloadHTML
optionsJSON = Rd.optionsJSON (\lens v → right ∘ (ModifyJSONOpts (lens .~ v)))

compress ∷ State → DownloadHTML
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

downloadButton ∷ State → DownloadHTML
downloadButton state =
  HH.a
    [ HP.classes [ B.btn, B.btnPrimary ]
    , HP.href $ fromMaybe "#" (url <$> state.source)
    , ARIA.label "Download"
    , HP.title "Download"
    ]
    [ HH.text "Download" ]
  where
  url ∷ PU.FilePath → String
  url file =
    (encodeURI (printPath Paths.data_ ⊕ printPath file)) ⊕ headersPart

  headersPart ∷ String
  headersPart =
   "?request-headers="
   ⊕ (Global.encodeURIComponent $ show $ reqHeadersToJSON $ D.toHeaders state)

eval ∷ Natural QueryP DownloadDSL
eval = coproduct cardEval downloadEval

cardEval ∷ Natural Ec.CardEvalQuery DownloadDSL
cardEval (Ec.EvalCard { inputPort } continue) = do
  case inputPort of
    Just (P.TaggedResource { resource }) → H.modify (_source ?~ resource)
    Just P.Blocked → H.modify (_source .~ Nothing)
    _ → pure unit
  pure $ continue { output: Just P.Blocked, messages: [] }
cardEval (Ec.NotifyRunCard next) = pure next
cardEval (Ec.NotifyStopCard next) = pure next
cardEval (Ec.Save k) = map (k ∘ encode) H.get
cardEval (Ec.Load json next) = for_ (decode json) H.set $> next
cardEval (Ec.SetupCard { inputPort } next) = do
  H.modify $ _source .~ preview P._Resource inputPort
  pure next
cardEval (Ec.SetCanceler _ next) = pure next

downloadEval ∷ Natural Query DownloadDSL
downloadEval (SetOutput ty next) = do
  options ← H.gets _.options
  case ty, options of
    D.CSV, (Right _) → H.modify _{options = Left D.initialCSVOptions}
    D.JSON, (Left _) → H.modify _{options = Right D.initialJSONOptions}
    _, _ → pure unit
  pure next
downloadEval (ModifyCSVOpts fn next) = H.modify (_options ∘ _Left %~ fn) $> next
downloadEval (ModifyJSONOpts fn next) = H.modify (_options ∘ _Right %~ fn) $> next
downloadEval (ToggleCompress next) = H.modify (_compress %~ not) $> next
