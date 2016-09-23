module SlamData.Workspace.Card.BuildChart.Graph.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Graph.Model
  ) where

import SlamData.Prelude


import Data.Argonaut (JArray, JCursor, Json, cursorGet, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Foreign as FR
import Data.Foreign.Class (readProp)
import Data.Int as Int
import Data.Lens ((^?))
import Data.Map as M
import Data.String as Str
import Data.String.Regex as Rgx

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP

import Global (infinity)

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Graph.Model (Model, GraphR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Graph))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.Axis (Axis, analyzeJArray)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.Semantics (analyzeJson, semanticsToNumber)
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Chart.Semantics as Sem
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port


eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  records ← BCE.records resource
  pure $ Port.ChartInstructions (buildGraph conf records) Graph


type EdgeItem = String × String

type GraphItem =
  { size ∷ Maybe Number
  , category ∷ Maybe String
  , value ∷ Maybe Number
  , source ∷ Maybe String
  , target ∷ Maybe String
  , name ∷ Maybe String
  }

type GraphData = Array GraphItem × Array EdgeItem

buildGraphData ∷ JArray → M.Map JCursor Axis → GraphR → GraphData
buildGraphData records axesMap r =
  nodes × edges
  where
  rawEdges ∷ Array EdgeItem
  rawEdges =
    A.nub $ A.catMaybes $ A.zipWith edgeZipper sources targets

  edgeZipper ∷ Maybe String → Maybe String → Maybe EdgeItem
  edgeZipper s t = Tuple <$> s <*> t

  sources ∷ Array (Maybe String)
  sources =
    foldMap (pure ∘ map Sem.printSemantics)
      $ foldMap Ax.runAxis
      $ M.lookup r.source axesMap

  targets ∷ Array (Maybe String)
  targets =
    foldMap (pure ∘ map Sem.printSemantics)
      $ foldMap Ax.runAxis
      $ M.lookup r.target axesMap

  nodes ∷ Array GraphItem
  nodes =
    A.nubBy (\r1 r2 → r1.name ≡ r2.name)
      $ map (\(source × target × category) →
              let
                value = M.lookup (source × category) valueMap
                size = map relativeSize value
              in
               { source
               , target
               , value
               , size
               , category
               , name:
                 (map (\s → "edge source:" ⊕ s) source)
                 ⊕ (map (\c → ":category:" ⊕ c) category)
              })
      $ A.zip (sources ⊕ nothingTail sources)
      $ A.zip (targets ⊕ nothingTail targets)
      $ categories ⊕ nothingTail categories

  edgeMap ∷ M.Map String (Array String)
  edgeMap = foldl foldFn M.empty nodes

  foldFn ∷ M.Map String (Array String) → GraphItem → M.Map String (Array String)
  foldFn acc { source, target, name} = case name of
    Nothing → acc
    Just n →
      maybe id (M.alter (alterFn n)) target
      $ maybe id (M.alter (alterFn n)) source
      $ acc

  alterFn ∷ String → Maybe (Array String) → Maybe (Array String)
  alterFn s Nothing = pure $ A.singleton s
  alterFn s (Just arr) = pure $ A.cons s arr

  maxLength ∷ Int
  maxLength =
    fromMaybe zero
    $ F.maximum
      [ A.length categories
      , A.length sources
      , A.length targets
      ]

  nothingTail ∷ ∀ a. Array (Maybe a) → Array (Maybe a)
  nothingTail heads = do
    guard (maxLength > A.length heads)
    map (const Nothing) $ A.range 0 (maxLength - A.length heads)

  categories ∷ Array (Maybe String)
  categories =
    foldMap (pure ∘ map Sem.printSemantics)
      $ foldMap Ax.runAxis
      $ r.color
      >>= flip M.lookup axesMap

  names ∷ Array (Maybe String)
  names = sources ⊕ targets

  minimumValue ∷ Number
  minimumValue = fromMaybe (-1.0 * infinity) $ F.minimum valueMap

  maximumValue ∷ Number
  maximumValue = fromMaybe infinity $ F.maximum valueMap

  distance ∷ Number
  distance = maximumValue - minimumValue

  sizeDistance ∷ Number
  sizeDistance = r.maxSize - r.minSize

  relativeSize ∷ Number → Number
  relativeSize val
    | val < 0.0 = 0.0
    | otherwise =
      r.maxSize - sizeDistance / distance * (maximumValue - val)

  edges ∷ Array EdgeItem
  edges = do
    (s × t) ← rawEdges
    ss ← fromMaybe [ ] $ M.lookup s edgeMap
    tt ← fromMaybe [ ] $ M.lookup t edgeMap
    pure $ ss × tt

  valueMap ∷ M.Map ((Maybe String) × (Maybe String)) Number
  valueMap = map (Ag.runAggregation $ fromMaybe Ag.Sum r.sizeAggregation) valueArrMap

  valueArrMap ∷ M.Map ((Maybe String) × (Maybe String)) (Array Number)
  valueArrMap =
    foldl valueFoldFn M.empty records

  valueFoldFn
    ∷ M.Map ((Maybe String) × (Maybe String)) (Array Number)
    → Json
    → M.Map ((Maybe String) × (Maybe String)) (Array Number)
  valueFoldFn acc js =
    let
      mbSource = toString =<< cursorGet r.source js
      mbCategory = toString =<< flip cursorGet js =<< r.color
      mbValue = semanticsToNumber =<< analyzeJson =<< flip cursorGet js =<< r.size

      valueAlterFn ∷ Maybe Number → Maybe (Array Number) → Maybe (Array Number)
      valueAlterFn (Just a) Nothing = Just [a]
      valueAlterFn (Just a) (Just arr) = Just $ A.cons a arr
      valueAlterFn _ mbArr = mbArr
    in
      M.alter (valueAlterFn mbValue) (mbSource × mbCategory) acc


sourceRgx ∷ Rgx.Regex
sourceRgx = unsafePartial fromRight $ Rgx.regex "source:([^:]+)" Rgx.noFlags

categoryRgx ∷ Rgx.Regex
categoryRgx = unsafePartial fromRight $ Rgx.regex "category:([^:]+)" Rgx.noFlags

buildGraph ∷ GraphR → JArray → DSL OptionI
buildGraph r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.formatterItem \{name, value, "data": item, dataType} →
      let
        fItem ∷ FR.Foreign
        fItem = FR.toForeign item

        mbSource ∷ Maybe String
        mbSource = join $ Rgx.match sourceRgx name >>= flip A.index 1

        mbCat ∷ Maybe String
        mbCat = join $ Rgx.match categoryRgx name >>= flip A.index 1

        mbVal ∷ Maybe Number
        mbVal = if FR.isUndefined $ FR.toForeign value then Nothing else Just value

        itemTooltip ∷ String
        itemTooltip =
          (foldMap (\s → "name: " ⊕ s) mbSource)
          ⊕ (foldMap (\c → "<br /> category: " ⊕ c) mbCat)
          ⊕ (foldMap (\v → "<br /> value: " ⊕ show v) mbVal)
          ⊕ (foldMap (\a → "<br /> value aggregation: "
                           ⊕ (Str.toLower $ Ag.printAggregation a))
             r.sizeAggregation)
      in fromMaybe itemTooltip do
        guard $ dataType ≡ "edge"
        source ← either (const Nothing) Just $ FR.readString =<< readProp "source" fItem
        target ← either (const Nothing) Just $ FR.readString =<< readProp "target" fItem
        sourceName ← Str.stripPrefix "edge " source
        targetName ← Str.stripPrefix "edge " target
        pure $ sourceName ⊕ " > " ⊕ targetName

  E.legend do
    E.orient ET.Vertical
    E.leftLeft
    E.topTop
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem legendNames

  E.colors colors

  E.series $ E.graph do
    if r.circular
      then E.layoutCircular
      else E.layoutForce

    E.force do
      E.edgeLength 120.0
      E.layoutAnimation true

    E.buildItems items
    E.buildLinks links

    E.buildCategories $ for_ legendNames $ E.addCategory ∘ E.name
    E.lineStyle $ E.normal $ E.colorSource

  where
  axisMap ∷ M.Map JCursor Axis
  axisMap = analyzeJArray records

  graphData ∷ GraphData
  graphData = buildGraphData records axisMap r

  legendNames ∷ Array String
  legendNames = A.nub $ A.catMaybes $ map _.category $ fst graphData

  links ∷ DSL ETP.LinksI
  links = for_ (snd graphData) \(sName × tName) → E.addLink do
    E.sourceName sName
    E.targetName tName

  items ∷ DSL ETP.ItemsI
  items = for_ (fst graphData) \item → E.addItem do
    for_ (item.category >>= flip A.elemIndex legendNames) E.category
    traverse_ E.symbolSize $ map Int.floor item.size
    E.value $ fromMaybe zero item.value
    traverse_ E.name item.name
    E.itemStyle $ E.normal do
      E.borderWidth 1
    E.label do
      E.normal E.hidden
      E.emphasis E.hidden
