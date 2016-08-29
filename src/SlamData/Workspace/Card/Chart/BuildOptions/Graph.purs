module SlamData.Workspace.Card.Chart.BuildOptions.Graph where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor)
import Data.Array as A
import Data.Int as Int
import Data.Foldable as F
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

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
  , name ∷ Maybe String
  }

type GraphData = Array GraphItem × Array EdgeItem

buildGraphData ∷ M.Map JCursor Axis → GraphR → GraphData
buildGraphData axesMap r =
  nodes × edges
  where
  edges ∷ Array EdgeItem
  edges =
    A.nub $ A.catMaybes $ A.zipWith edgeZipper sources targets

  edgeZipper ∷ Maybe String → Maybe String → Maybe (String × String)
  edgeZipper source target = Tuple <$> source <*> target

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
      $ map (\(size × category × name) →
              { size
              , category
              , name: name
                  ⊕ (map (\c → ":category:" ⊕ c) category)
                  ⊕ (map (\s → ":size:" ⊕ show s) size)
              })
      $ A.zip (sizes ⊕ nothingTail sizes)
      $ A.zip (categories ⊕ nothingTail categories)
      $ names ⊕ nothingTail names

  maxLength ∷ Int
  maxLength = fromMaybe zero $ F.maximum [ A.length categories, A.length names, A.length sizes ]

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

  sizes ∷ Array (Maybe Number)
  sizes =
    foldMap (pure ∘ flip bind Sem.semanticsToNumber)
    $ foldMap Ax.runAxis
    $ r.size
    >>= flip M.lookup axesMap


buildGraph ∷ GraphR → JArray → DSL OptionI
buildGraph r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

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
      E.edgeLength 50.0
      E.repulsion 100.0
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
    traverse_ E.value item.size
    traverse_ E.name item.name
    E.label do
      E.normal E.hidden
      E.emphasis E.hidden
