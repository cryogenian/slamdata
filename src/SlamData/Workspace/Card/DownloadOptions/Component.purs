module SlamData.Workspace.Card.DownloadOptions.Component where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Lens ((?~), (.~), preview, _Left, _Right, (%~))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.Common.EvalQuery as Ec
import SlamData.Workspace.Card.Component as Cc
import SlamData.Workspace.Card.Component (makeCardComponent, makeQueryPrism, _DownloadOptionsState, _DownloadOptionsQuery)
import SlamData.Workspace.Card.DownloadOptions.Component.Query (QueryP, Query(..))
import SlamData.Workspace.Card.DownloadOptions.Component.State (State, _compress, _options, _source, initialState, encode, decode)
import SlamData.Render.CSS as Rc
import SlamData.Effects (Slam)
import SlamData.Download.Model as D
import SlamData.Download.Render as Rd
import SlamData.Workspace.Card.CardType (CardType(DownloadOptions))
import SlamData.Workspace.Card.Port as P
import SlamData.Render.Common (row)

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

renderDownloadConfiguration ∷ State → HTML
renderDownloadConfiguration state =
  HH.div
    [ HP.classes [ Rc.downloadConfiguration ] ]
    $ [ either optionsCSV optionsJSON state.options ]
    ⊕ [ row
          [ HH.div [ HP.classes [ B.colXs4 ] ] [ compress state ]
          ]
      ]

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
cardEval (Ec.EvalCard {inputPort} continue) = do
  map continue $ Ec.runCardEvalT do
    case inputPort of
      Just P.Blocked → lift do
        H.modify (_source .~ Nothing)
        pure $ Just P.Blocked
      Just (P.TaggedResource {resource}) → lift do
        H.modify (_source ?~ resource)
        state ← H.get
        pure
          $ Just
          $ P.DownloadOptions
              { resource
              , compress: state.compress
              , options: state.options
              }
      _ → throwError "Incorrect input in download options card"
cardEval (Ec.NotifyRunCard next) = pure next
cardEval (Ec.NotifyStopCard next) = pure next
cardEval (Ec.Save k) = map (k ∘ encode) H.get
cardEval (Ec.Load json next) = for_ (decode json) H.set $> next
cardEval (Ec.SetupCard {inputPort} next) = do
  H.modify $ _source .~ preview P._Resource inputPort
  pure next
cardEval (Ec.SetCanceler _ next) = pure next

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
