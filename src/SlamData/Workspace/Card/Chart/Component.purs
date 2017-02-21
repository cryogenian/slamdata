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

module SlamData.Workspace.Card.Chart.Component (chartComponent) where

import SlamData.Prelude

import Data.Array as A
import Data.Foreign as F
import Data.Foreign.Class (readProp)
import Data.Int (toNumber, floor)
import Data.String as S

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.ECharts as HEC
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import SlamData.Render.CSS as RC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType (ChartType, chartDarkIconSrc)
import SlamData.Workspace.Card.CardType.ChartType as ChT
import SlamData.Workspace.Card.Chart.Component.ChildSlot (ChildQuery, ChildSlot, cpECharts, cpMetric, cpPivotTable)
import SlamData.Workspace.Card.Chart.Component.State (State, initialState)
import SlamData.Workspace.Card.Chart.Component.Query (Query(..))
import SlamData.Workspace.Card.Chart.MetricRenderer.Component as Metric
import SlamData.Workspace.Card.Chart.Model as Chart
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Component as Pivot
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port (Port(..), extractResource)
import SlamData.Workspace.LevelOfDetails as LOD

type HTML = CC.InnerCardParentHTML Query ChildQuery ChildSlot
type DSL = CC.InnerCardParentDSL State Query ChildQuery ChildSlot

chartComponent ∷ CC.CardOptions → CC.CardComponent
chartComponent =
  CC.makeCardComponent CT.Chart $ H.parentComponent
    { render: render
    , eval: evalCard ⨁ evalComponent
    , initialState: const initialState
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ RC.chartOutput, HH.ClassName "card-input-maximum-lod" ] ]
    case state.chartType of
      Just ChT.Metric →
        [ HH.slot' cpMetric unit Metric.comp state.dimensions absurd ]
      Just ChT.PivotTable →
        [ HH.slot' cpPivotTable unit Pivot.component unit (Just ∘ right <$> handlePivotTableMessage) ]
      _ →
        [ HH.slot' cpECharts unit HEC.echarts (Tuple (state.dimensions { height = state.dimensions.height - 60 }) unit) (const Nothing) ]

renderButton ∷ ChartType → Array HTML
renderButton ct =
  [ HH.img [ HP.src $ chartDarkIconSrc ct ]
  , HH.text "Zoom or resize"
  ]

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    H.gets _.chartType >>= case _ of
      Just ChT.PivotTable → do
        res ← H.query' cpPivotTable unit $ H.request Pivot.Save
        pure $ k (Card.Chart (Chart.PivotTableRenderer <$> res))
      _ →
        pure $ k (Card.Chart Nothing)
  CC.Load model next →
    case model of
      Card.Chart (Just (Chart.PivotTableRenderer m)) → do
        H.modify (_ { chartType = Just ChT.PivotTable })
        H.query' cpPivotTable unit $ H.action $ Pivot.Load m
        pure next
      _ →
        pure next
  CC.ReceiveInput input varMap next → do
    case input, extractResource varMap of
      ChartInstructions r, _ → void do
        H.modify (_ { chartType = Just r.chartType })
        H.query' cpECharts unit $ H.action $ HEC.Reset r.options
        H.query' cpECharts unit $ H.action HEC.Resize
      ValueMetric metric, _ → void do
        H.modify (_ { chartType = Just ChT.Metric })
        H.query' cpMetric unit $ H.action $ Metric.SetMetric metric
      PivotTable port, Just resource → void do
        H.modify (_ { chartType = Just ChT.PivotTable })
        H.query' cpPivotTable unit $ H.action $ Pivot.Update port resource
      _, _ →
        void $ H.query' cpECharts unit $ H.action HEC.Clear
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions dims reply → do
    state ← H.get
    let
      widthPadding = 6
      intWidth = floor dims.width - widthPadding
      intHeight = floor dims.height
    H.modify (_ { dimensions = { width: intWidth, height: intHeight } })
    reply <$> maybe (pure LOD.High) lodByChartType state.chartType


lodByChartType ∷ ChT.ChartType → DSL LOD.LevelOfDetails
lodByChartType = case _ of
  ChT.Metric →
    fromMaybe LOD.Low <$> H.query' cpMetric unit (H.request Metric.GetLOD)
  ChT.PivotTable →
    pure LOD.High
  _ → do
    { width, height } ← H.gets _.dimensions
    mbOpts ← H.query' cpECharts unit $ H.request HEC.GetOptions
    let
      eToM ∷ ∀ a. F.F a → Maybe a
      eToM = either (const Nothing) Just ∘ runExcept

      eBottom = do
        fOption ← join mbOpts
        grids ← eToM $ readProp "grid" fOption
        grid ← A.head grids
        eToM $ readProp "bottom" grid

      eBottomPx = eToM ∘ F.readInt =<< eBottom

      eBottomPct = do
        pctStr ← eToM ∘ F.readString =<< eBottom
        str ← S.stripSuffix (S.Pattern "%") pctStr
        let num = readFloat str
        guard (not $ isNaN num)
        pure $ floor $ num / 100.0 * toNumber height

    fromMaybe LOD.Low <$> for (eBottomPx <|> eBottomPct <|> pure zero) \bottomPx →
      pure
        if (height - bottomPx) < 200 ∨ width < 300
        then LOD.Low
        else LOD.High

evalComponent ∷ Query ~> DSL
evalComponent = case _ of
  RaiseUpdate next → do
    H.raise CC.modelUpdate
    pure next

handlePivotTableMessage ∷ Pivot.Message → Query Unit
handlePivotTableMessage = case _ of
  Pivot.ModelUpdated → H.action RaiseUpdate
