{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.DimensionMap.Defaults
  ( ProjectionDefaults
  , defaults
  ) where

import SlamData.Prelude
import Data.Argonaut (JCursor)
import Data.Array as A
import Data.Lens ((?~))
import Data.StrMap as SM
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Package.DSL as T
import SlamData.Workspace.Card.Setups.Package.Projection as Pr
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

type ProjectionDefaults =
  { dimension ∷ JCursor → D.LabeledJCursor
  , label ∷ String
  , value ∷ String
  , select ∷ String
  }

jcursorProjection ∷ JCursor → D.Dimension Void JCursor
jcursorProjection = D.projection

defaults ∷ SM.StrMap ProjectionDefaults
defaults = foldr insertProjection SM.empty (statics <> dynamics)
  where
  insertProjection (prj × val) =
    T.unpackProjection prj ?~ val

statics ∷ Array (T.Projection × ProjectionDefaults)
statics =
  [ Pr._dimension ×
      { dimension: jcursorProjection
      , label: "Dimension"
      , value: "Choose dimension"
      , select: "Choose dimension"
      }
  , Pr._high ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "High"
      , value: "Choose high"
      , select: "Choose measure for high position"
      }
  , Pr._low ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Low"
      , value: "Choose low"
      , select: "Choose measure for low position"
      }
  , Pr._open ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Open"
      , value: "Choose open"
      , select: "Choose measure for open position"
      }
  , Pr._close ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Close"
      , value: "Choose close"
      , select: "Choose measure for close position"
      }
  , Pr._parallel ×
      { dimension: jcursorProjection
      , label: "Parallel"
      , value: "Choose parallel"
      , select: "Choose parallel"
      }
  , Pr._value ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Measure #1"
      , value: "Choose measure"
      , select: "Choose measure"
      }
  , Pr._series ×
      { dimension: jcursorProjection
      , label: "Series"
      , value: "Choose series"
      , select: "Choose series"
      }
  , Pr._category ×
      { dimension: jcursorProjection
      , label: "Category"
      , value: "Choose category"
      , select: "Choose category"
      }
  , Pr._stack ×
      { dimension: jcursorProjection
      , label: "Stack"
      , value: "Choose stack"
      , select: "Choose stack"
      }
  , Pr._source ×
      { dimension: jcursorProjection
      , label: "Source"
      , value: "Choose source"
      , select: "Choose source"
      }
  , Pr._target ×
      { dimension: jcursorProjection
      , label: "Target"
      , value: "Choose target"
      , select: "Choose target"
      }
  , Pr._abscissa ×
      { dimension: jcursorProjection
      , label: "X-axis"
      , value: "Choose x-axis"
      , select: "Choose x-axis"
      }
  , Pr._ordinate ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Y-axis"
      , value: "Choose y-axis"
      , select: "Choose y-axis"
      }
  , Pr._secondValue ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Measure #2"
      , value: "Choose measure"
      , select: "Choose measure"
      }
  , Pr._donut ×
      { dimension: jcursorProjection
      , label: "Donut"
      , value: "Choose donut"
      , select: "Choose donut"
      }
  , Pr._multiple ×
      { dimension: jcursorProjection
      , label: "Multiple"
      , value: "Choose multiple"
      , select: "Choose multiple"
      }
  , Pr._size ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Size"
      , value: "Choose size"
      , select: "Choose size"
      }
  , Pr._color ×
      { dimension: jcursorProjection
      , label: "Color"
      , value: "Choose color"
      , select: "Choose color"
      }
  ]

dynamics ∷ Array (T.Projection × ProjectionDefaults)
dynamics = A.range 0 30 <#> \ix →
  Pr._dimIx ix ×
    { dimension: jcursorProjection
    , label: "Measure #" <> show (ix + 1)
    , value: "Choose measure"
    , select: "Choose measure"
    }
