module SlamData.Workspace.Card.Chart.MetricRenderer.Component where

import SlamData.Prelude

import CSS as C
import CSS.TextAlign (textAlign, center)

import Data.Int as Int

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.CSS.Indexed as HC

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Chart.BuildOptions.Metric (Metric)

import Utils.DOM (getTextWidthPure, getTextHeightPure)

type State =
  { width ∷ Int
  , height ∷ Int
  , label ∷ Maybe String
  , value ∷ String
  , valueFontSize ∷ Int
  , labelFontSize ∷ Int
  }

initialState ∷ State
initialState =
  { width: 600
  , height: 400
  , label: Nothing
  , value: ""
  , valueFontSize: 0
  , labelFontSize: 0
  }

data Query a
  = SetMetric Metric a
  | SetDimensions {width ∷ Int, height ∷ Int} a
  | GetLOD (LevelOfDetails → a)

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state =
  HH.div_
    $ [ HH.div
        [ HP.classes [ HH.className "metric-value" ]
        , HC.style do
             C.fontSize $ C.px $ Int.toNumber state.valueFontSize
             C.position C.absolute
             C.width $ C.pct 100.0
             textAlign center
             C.top
               $ C.px
               $ Int.toNumber
               $ max zero
               $ (state.height - totalHeight) / 2

        ]
        [ HH.text state.value ] ]
    ⊕ foldMap renderLabel state.label
  where
  labelHeight ∷ Int
  labelHeight =
    Int.floor
    $ fromMaybe zero
    $ ubuntuHeight state.labelFontSize
    <$> state.label

  valueHeight ∷ Int
  valueHeight =
    Int.floor
    $ ubuntuHeight state.valueFontSize state.value

  totalHeight ∷ Int
  totalHeight =
    valueHeight + labelHeight

  valueWidth ∷ Int
  valueWidth =
    Int.floor
    $ ubuntuWidth state.valueFontSize state.value

  labelWidth ∷ Int
  labelWidth =
    Int.floor
    $ fromMaybe zero
    $ ubuntuWidth state.labelFontSize
    <$> state.label

  renderLabel ∷ String → Array HTML
  renderLabel str =
    [ HH.div
        [ HP.classes [ HH.className "metric-label" ]
        , HC.style do
             C.fontSize $ C.px $ Int.toNumber state.labelFontSize
             C.position C.absolute
             C.width $ C.pct 100.0
             textAlign center
             C.top
               $ C.px
               $ Int.toNumber
               $ max valueHeight
               $ (state.height + valueHeight - labelHeight) / 2
        ]
        [ HH.text str ]
    ]

eval ∷ Query ~> DSL
eval (SetMetric m next) = do
  H.modify _{label = m.label, value = m.value}
  adjustFontSizes
  pure next
eval (SetDimensions dims next) = do
  H.modify _{width = dims.width, height = dims.height}
  adjustFontSizes
  pure next
eval (GetLOD continue) = do
  state ← H.get
  pure $ continue $ if state.height < 120 ∨ state.valueFontSize < 17 then Low else High

adjustFontSizes ∷ DSL Unit
adjustFontSizes = do
  byWidth ← suggestFontSizesByWidth
  byHeight ← suggestFontSizesByHeight


  let
    valueFont = min byWidth.value byHeight.value
    labelFontRaw = Int.toNumber $ min byWidth.label byHeight.label
    labelFont = Int.floor $ min labelFontRaw (0.66 * Int.toNumber valueFont)

  H.modify _{ valueFontSize = valueFont
            , labelFontSize = labelFont
            }


suggestFontSizesByWidth ∷ DSL {value ∷ Int, label ∷ Int}
suggestFontSizesByWidth = do
  st ← H.get
  suggestDims (Int.toNumber st.width - 48.0)
  where
  suggestDims ∷ Number → DSL {value ∷ Int, label ∷ Int}
  suggestDims maxDim = do
    st ← H.get
    let
      value =
        if st.value ≠ ""
        then determineByWidth maxDim 40 st.value
        else zero
      label =
        if st.label ≠ Nothing ∧ st.label ≠ Just ""
        then determineByWidth maxDim 24 $ fromMaybe "" st.label
        else zero

    pure {value, label}

suggestFontSizesByHeight ∷ DSL {value ∷ Int, label ∷ Int}
suggestFontSizesByHeight = do
  st ← H.get
  pure
    $ determineByHeight
        (Int.toNumber st.height - 32.0)
        40
        {value: st.value, label: fromMaybe "" st.label}

determineByWidth ∷ Number → Int → String → Int
determineByWidth maxDim current text
  | text ≡ "" = 12
  | otherwise =
    let
      width = ubuntuWidth current text

      ratio = maxDim / width

      result
        | ratio < 1.05 ∧ ratio > 0.9 = current
        | otherwise =
          determineByWidth maxDim (Int.round $ Int.toNumber current * ratio) text
    in
      result

determineByHeight ∷ Number → Int → {value ∷ String, label ∷ String} → {value ∷ Int, label ∷ Int}
determineByHeight maxDim current input@{value, label}
  | value ≡ "" = {value: 12, label: 8}
  | otherwise =
    let
      labelCurrent = Int.ceil $ 0.66 * Int.toNumber current
      height = ubuntuHeight current value + ubuntuHeight labelCurrent label

      ratio = maxDim / height

      result
        | ratio < 1.05 ∧ ratio > 0.9 = {value: current, label: labelCurrent}
        | otherwise =
          determineByHeight maxDim (Int.round $ Int.toNumber current * ratio) input
    in
      result


ubuntuHeight ∷ Int → String → Number
ubuntuHeight fontSize text =
  getTextHeightPure
    { fontSize
    , text
    , fontFamily: "Ubuntu"
    , fontStyle: "normal"
    }

ubuntuWidth ∷ Int → String → Number
ubuntuWidth fontSize text =
  getTextWidthPure text $ "normal " ⊕ show fontSize ⊕ "px Ubuntu"
