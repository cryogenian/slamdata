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

import Utils.DOM (getTextWidthPure)

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
        ]
        [ HH.text state.value ] ]
    ⊕ foldMap renderLabel state.label
  where
  renderLabel ∷ String → Array HTML
  renderLabel str =
    [ HH.div
        [ HP.classes [ HH.className "metric-label" ]
        , HC.style do
             C.fontSize $ C.px $ Int.toNumber state.labelFontSize
             textAlign center
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
    minWidthVal = getTextWidthPure state.value "normal 16px Ubuntu"
    minWidthLbl = flip getTextWidthPure "normal 12px Ubuntu" <$> state.label
    widthNum = Int.toNumber state.width
    lod =
      if state.width < 24 ∨ widthNum < minWidthVal ∨ widthNum < fromMaybe zero minWidthLbl
      then Low
      else High
  pure $ continue lod

adjustFontSizes ∷ DSL Unit
adjustFontSizes = do
  st ← H.get
  let
    maxWidth = st.width - 24
  when (st.value ≠ "") do
    let valueFont = determineFont maxWidth 40 st.value
    H.modify _{valueFontSize = valueFont}

    when (st.label ≠ Nothing ∧ st.label ≠ Just "") do
      let
        labelFontRaw = determineFont maxWidth 24 $ fromMaybe "" st.label
        labelFontNum = Int.toNumber labelFontRaw
        valueFontNum = Int.toNumber valueFont
        labelFont =
          if labelFontNum > 0.66 * valueFontNum
          then Int.floor $ valueFontNum * 0.66
          else labelFontRaw
      H.modify _{labelFontSize = labelFont}


determineFont ∷ Int → Int → String → Int
determineFont maxWidth current text
  | text ≡ "" = 12
  | otherwise =
    let
      widthNum =
        getTextWidthPure text $ "normal " ⊕ show current ⊕ "px Ubuntu"
      maxWidthNum =
        Int.toNumber maxWidth

      ratio =
        maxWidthNum / widthNum

      result
        | ratio < 1.05 ∧ ratio > 0.9 = current
        | otherwise =
          determineFont maxWidth (spy $ Int.round $ Int.toNumber current  * ratio) text
    in
     result
