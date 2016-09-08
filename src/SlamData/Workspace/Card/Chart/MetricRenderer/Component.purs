module SlamData.Workspace.Card.Chart.MetricRenderer.Component where

import SlamData.Prelude

import CSS as C
import CSS.TextAlign (textAlign, center)

import Data.Foldable as F
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
             textAlign center
             C.position C.relative
             C.top
               $ C.px
               $ Int.toNumber
               $ (state.height - margins - totalHeight) / 2
        ]
        [ HH.text state.value ] ]
    ⊕ foldMap renderLabel state.label
  where
  margins ∷ Int
  margins = 48

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

  renderLabel ∷ String → Array HTML
  renderLabel str =
    [ HH.div
        [ HP.classes [ HH.className "metric-label" ]
        , HC.style do
             C.fontSize $ C.px $ Int.toNumber state.labelFontSize
             textAlign center
             C.position C.relative
             C.top
               $ C.px
               $ Int.toNumber
               $ (state.height - margins - labelHeight) / 2
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
  let
    minWidthVal = ubuntuWidth 16 state.value
    minWidthLbl = ubuntuWidth 12 <$> state.label
    minHeightVal = ubuntuHeight 16 state.value
    minHeightLbl = ubuntuHeight 12 $ fromMaybe "" state.label

    widthNum = Int.toNumber state.width
    heightNum = Int.toNumber state.height

    shouldBeLowLOD =
      F.or
        [ state.width < 24
        , widthNum < minWidthVal
        , widthNum < fromMaybe zero minWidthLbl
        , heightNum < minHeightVal + minHeightLbl
        ]

    lod = if shouldBeLowLOD then Low else High
  pure $ continue lod

adjustFontSizes ∷ DSL Unit
adjustFontSizes = do
  byWidth ← suggestFontSizesByWidth
  byHeight ← suggestFontSizesByHeight

  let
    valueFont = min byWidth.value byHeight.value
    labelFontRaw = Int.toNumber $ min byWidth.value byHeight.value
    labelFont = Int.floor $ min labelFontRaw (0.66 * Int.toNumber valueFont)

  H.modify _{ valueFontSize = valueFont
            , labelFontSize = labelFont
            }

suggestDims
  ∷ Number
  → (Number → Int → String → Int)
  → DSL {value ∷ Int, label ∷ Int}
suggestDims maxDim computeHeightFn = do
  st ← H.get
  let
    value =
      if st.value ≠ ""
      then computeHeightFn maxDim 40 st.value
      else zero

    label =
      if st.label ≠ Nothing ∧ st.label ≠ Just ""
      then computeHeightFn maxDim 24 $ fromMaybe "" st.label
      else zero

  pure {value, label}


suggestFontSizesByWidth ∷ DSL {value ∷ Int, label ∷ Int}
suggestFontSizesByWidth = do
  st ← H.get
  suggestDims (Int.toNumber $ st.width - 24) determineWidth

suggestFontSizesByHeight ∷ DSL {value ∷ Int, label ∷ Int}
suggestFontSizesByHeight = do
  st ← H.get
  suggestDims (Int.toNumber st.height) determineHeight

determineBy ∷ (Int → String → Number) → Number → Int → String → Int
determineBy getHeight maxDim current text
  | text ≡ "" = 12
  | otherwise =
    let
      height = getHeight current text

      ratio = maxDim / height

      result
        | ratio < 1.05 ∧ ratio > 0.9 = current
        | otherwise =
          determineBy getHeight maxDim (Int.round $ Int.toNumber current * ratio) text
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

determineHeight ∷ Number → Int → String → Int
determineHeight =
  determineBy ubuntuHeight


determineWidth ∷ Number → Int → String → Int
determineWidth =
  determineBy ubuntuWidth
