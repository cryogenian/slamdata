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

module SlamData.Workspace.Card.Chart.PivotTableRenderer.Component where

import SlamData.Prelude
import Data.Argonaut as J
import Data.Array as Array
import Data.Foldable as F
import Data.List (List, (:))
import Data.List as List
import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model (PivotTableR, Column(..))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Model as PTRM

type State =
  { records ∷ Array J.Json
  , options ∷ PivotTableR
  , model ∷ PTRM.Model
  }

initialState ∷ State
initialState =
  { records: []
  , options: { dimensions: [], columns: [] }
  , model: PTRM.initialModel
  }

data Query a
  = Update (Array J.Json) PivotTableR a
  | Load PTRM.Model a
  | Save (PTRM.Model → a)
  | ModelUpdated a

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render st =
  HH.div
    [ HP.classes [ HH.className "sd-pivot-table" ] ]
    [ case Array.length st.options.dimensions, Array.length st.options.columns of
        _, 0 → HH.text ""
        0, _ → renderFlat st.options.columns st.records
        _, _ → renderTable st.options.dimensions st.options.columns st.records
    ]
  where
  renderFlat cols rows =
    HH.table_ $
      [ HH.tr_ (map (\(Column { value }) → HH.th_ [ HH.text (show value) ]) cols)
      ] <> map HH.tr_ (renderLeaves (Array.mapWithIndex Tuple cols) rows)

  renderTable dims cols rows =
    let
      dims' = List.fromFoldable (Array.mapWithIndex (\i _ → J.cursorGet (tupleN i)) dims)
      cols' = Array.mapWithIndex (Tuple ∘ add (Array.length dims)) cols
    in
      HH.table_ $
        [ HH.tr_ $
            [ HH.td [ HP.colSpan (Array.length dims) ] []
            ] <> map (\(Column { value }) → HH.th_ [ HH.text (show value) ]) cols
        ] <> renderRows dims' cols' rows

  renderRows dims cols =
    map HH.tr_ <<< buildTree dims (renderLeaves cols) renderHeadings

  renderLeaves cols =
    foldMap (renderLeaf cols)

  renderLeaf cols row =
    let
      rowLen =
        fromMaybe 1
          (F.maximum
            (Array.mapMaybe
              (map (J.foldJsonArray 1 Array.length)
                <<< flip J.cursorGet row
                <<< tupleN
                <<< fst)
              cols))
    in
      Array.range 0 (rowLen - 1) <#> \rowIx →
        flip foldMap cols \(ix × Column { valueAggregation }) →
          let
            text = J.cursorGet (tupleN ix) row <#> case rowIx, valueAggregation of
              0, Just ag →
                foldJsonArray'
                  renderJson
                  (show <<< Ag.runAggregation ag <<< jsonNumbers)
              _, _ →
                foldJsonArray'
                  (const "")
                  (maybe "" renderJson <<< flip Array.index rowIx)
          in
            [ HH.td_ [ HH.text (fromMaybe "" text) ] ]

  jsonNumbers =
    Array.mapMaybe (J.foldJsonNumber Nothing Just)

  renderHeadings =
    foldMap renderHeading

  renderHeading (k × rs) =
    case Array.uncons rs of
      Just { head, tail } →
        Array.cons
          (Array.cons
            (HH.th [ HP.rowSpan (Array.length rs) ] [ HH.text (renderJson k) ])
            head)
          tail
      Nothing →
        []

  renderJson =
    J.foldJson show show show id show show

eval ∷ Query ~> DSL
eval = case _ of
  Update records options next → do
    H.modify _
      { records = records
      , options = options
      }
    pure next
  Load model next → do
    H.modify _
      { model = model
      }
    pure next
  Save k →
    k <$> H.gets _.model
  ModelUpdated next →
    pure next

tupleN ∷ Int → J.JCursor
tupleN int = J.JField ("_" <> show int) J.JCursorTop

data PTree k a
  = Bucket (Array a)
  | Grouped (Array (k × PTree k a))

foldTree
  ∷ ∀ k a r
  . (Array a → r)
  → (Array (k × r) → r)
  → PTree k a
  → r
foldTree f g (Bucket a) = f a
foldTree f g (Grouped as) = g (map (foldTree f g) <$> as)

buildTree
  ∷ ∀ k a r
  . Eq k
  ⇒ List (a → Maybe k)
  → (Array a → r)
  → (Array (k × r) → r)
  → Array a
  → r
buildTree List.Nil f g as = f as
buildTree (k : ks) f g as =
  g (fin (foldl go { key: Nothing, group: [], acc: [] } as))
  where
  go res@{ key: mbKey, group, acc } a =
    case mbKey, k a of
      Just key, Just key' | key == key' →
        { key: mbKey, group: Array.snoc group a, acc }
      _, Just key' →
        { key: Just key', group: [a], acc: fin res }
      _, Nothing →
        res
  fin { key, group, acc } =
    case key of
      Just key' →
        Array.snoc acc (key' × (buildTree ks f g group))
      Nothing →
        acc

foldJsonArray'
  ∷ ∀ a
  . (J.Json → a)
  → (J.JArray → a)
  → J.Json
  → a
foldJsonArray' f g j = J.foldJson f' f' f' f' g f' j
  where
  f' ∷ ∀ b. b → a
  f' _ = f j
