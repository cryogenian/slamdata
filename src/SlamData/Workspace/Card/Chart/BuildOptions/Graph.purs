module SlamData.Workspace.Card.Chart.BuildOptions.Graph where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor)
import Data.Array as A
import Data.Int as Int
import Data.Foldable as F
import Data.Map as M
import Data.String.Regex as Rgx

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Global (infinity)

import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Chart.Semantics as Sem

type GraphR =
  { source ∷ JCursor
  , target ∷ JCursor
  , size ∷ Maybe JCursor
  , color ∷ Maybe JCursor
  , minSize ∷ Number
  , maxSize ∷ Number
  , circular ∷ Boolean
  , axes ∷ Axes
  }


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

buildGraphData ∷ M.Map JCursor Axis → GraphR → GraphData
buildGraphData axesMap r =
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
      $ map (\(source × target × value × size × category) →
              { source
              , target
              , value
              , size
              , category
              , name:
                  (map (\s → "edge source:" ⊕ s) source)
                  ⊕ (map (\c → ":category:" ⊕ c) category)
                  ⊕ (map (\s → ":size:" ⊕ show s) size)
              })
      $ A.zip (sources ⊕ nothingTail sources)
      $ A.zip (targets ⊕ nothingTail targets)
      $ A.zip (values ⊕ nothingTail values)
      $ A.zip (sizes ⊕ nothingTail sizes)
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
      , A.length sizes
      , A.length targets
      , A.length values
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

  values ∷ Array (Maybe Number)
  values =
    foldMap (pure ∘ flip bind Sem.semanticsToNumber)
    $ foldMap Ax.runAxis
    $ r.size
    >>= flip M.lookup axesMap

  minimumValue ∷ Number
  minimumValue = fromMaybe (-1.0 * infinity) $ F.minimum $ A.catMaybes values

  maximumValue ∷ Number
  maximumValue = fromMaybe infinity $ F.maximum $ A.catMaybes values

  distance ∷ Number
  distance = maximumValue - minimumValue

  sizeDistance ∷ Number
  sizeDistance = r.maxSize - r.minSize

  relativeSize ∷ Number → Number
  relativeSize val
    | val < 0.0 = 0.0
    | otherwise =
      sizeDistance / distance * (maximumValue - val)  + r.minSize

  sizes ∷ Array (Maybe Number)
  sizes = map (map relativeSize) values

  edges ∷ Array EdgeItem
  edges = do
    (s × t) ← rawEdges
    ss ← fromMaybe [ ] $ M.lookup s edgeMap
    tt ← fromMaybe [ ] $ M.lookup t edgeMap
    pure $ ss × tt


sourceRgx ∷ Rgx.Regex
sourceRgx = unsafePartial fromRight $ Rgx.regex "source:(\\w+)" Rgx.noFlags

categoryRgx ∷ Rgx.Regex
categoryRgx = unsafePartial fromRight $ Rgx.regex "category:(\\w+)" Rgx.noFlags

buildGraph ∷ GraphR → JArray → DSL OptionI
buildGraph r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.formatterItem \{name, value} →
      let
        mbSource ∷ Maybe String
        mbSource = join $ Rgx.match sourceRgx name >>= flip A.index 1

        mbCat ∷ Maybe String
        mbCat = join $ Rgx.match categoryRgx name >>= flip A.index 1
      in
        (foldMap (\s → "source: " ⊕ s) mbSource)
        ⊕ (foldMap (\c → "<br /> category: " ⊕ c) mbCat)
        ⊕ "<br /> value: " ⊕ show value

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
      E.edgeLength 100.0
      E.repulsion 200.0
      E.gravity 0.2
      E.layoutAnimation true

    E.circular do
      E.rotateLabel true

    E.buildItems items
    E.buildLinks links

    E.buildCategories $ for_ legendNames $ E.addCategory ∘ E.name
    E.lineStylePair $ E.normal $ E.colorSource

  where
  axisMap ∷ M.Map JCursor Axis
  axisMap = analyzeJArray records

  graphData ∷ GraphData
  graphData = buildGraphData axisMap r

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
