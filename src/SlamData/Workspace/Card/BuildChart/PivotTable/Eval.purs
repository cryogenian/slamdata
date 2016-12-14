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

module SlamData.Workspace.Card.BuildChart.PivotTable.Eval
  ( eval
  , escapedCursor
  , module PTM
  ) where

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Throw (class MonadThrow)

import Data.Argonaut as J
import Data.Array as Array
import Data.Path.Pathy as P
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RXF

import SlamData.Prelude
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

import Utils.Path (FilePath)

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Port.TaggedResourcePort
  → PTM.Model
  → m Port.Port
eval tr options = do
  state ← get
  axes ←
    case state of
      Just (CEM.Analysis { axes: ax, taggedResource })
        | Port.eqTaggedResourcePort tr taggedResource → pure ax
      _ → CEM.liftQ (QQ.axes tr.resource 100)
  let
    path = fromMaybe P.rootDir (P.parentDir tr.resource)
    query = mkSql options tr.resource
    state' =
      { axes
      , records: []
      , taggedResource: tr
      }
    port =
      { options
      , query
      , taggedResource: tr
      }
  put (Just (CEM.Analysis state'))
  pure (Port.PivotTable port)

mkSql ∷ PTM.Model → FilePath → String
mkSql options resource =
  let
    isSimple =
      PTM.isSimple options
    dimLen =
      Array.length options.dimensions
    groupBy =
      map (\value → "row" <> escapedCursor value) options.dimensions
    dims =
      Array.mapWithIndex
        (\i value → "row" <> escapedCursor value <> " AS _" <> show i)
        options.dimensions
    cols =
      Array.mapWithIndex
        case _, _ of
          i, PTM.Column c | isSimple → "row" <> escapedCursor c.value <> " AS _" <> show (i + dimLen)
          i, PTM.Column c → sqlAggregation c.valueAggregation ("row" <> escapedCursor c.value) <> " AS _" <> show (i + dimLen)
          i, PTM.Count    → "COUNT(*) AS _" <> show (i + dimLen)
        options.columns
    head =
      [ "SELECT " <> String.joinWith ", " (dims <> cols)
      , "FROM {{path}} AS row"
      ]
    tail =
      [ "GROUP BY " <> String.joinWith ", " groupBy
      , "ORDER BY " <> String.joinWith ", " groupBy
      ]
  in
    QQ.templated resource $
      String.joinWith " "
        if dimLen == 0
          then head
          else head <> tail

tickRegex ∷ Regex.Regex
tickRegex = unsafePartial (fromRight (Regex.regex "`" RXF.global))

escapeField ∷ String → String
escapeField str = "`" <> Regex.replace tickRegex "\\`" str <> "`"

escapedCursor ∷ J.JCursor → String
escapedCursor = case _ of
  J.JField c cs → "." <> escapeField c <> escapedCursor cs
  J.JIndex c cs → "[" <> show c <> "]" <> escapedCursor cs
  J.JCursorTop  → ""

sqlAggregation ∷ Maybe Ag.Aggregation → String → String
sqlAggregation a b = case a of
  Just Ag.Minimum → "MIN(" <> b <> ")"
  Just Ag.Maximum → "MAX(" <> b <> ")"
  Just Ag.Average → "AVG(" <> b <> ")"
  Just Ag.Sum     → "SUM(" <> b <> ")"
  _ → "[" <> b <> " ...]"
