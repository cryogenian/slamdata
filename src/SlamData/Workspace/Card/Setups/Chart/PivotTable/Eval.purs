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

module SlamData.Workspace.Card.Setups.Chart.PivotTable.Eval
  ( eval
  , module PTM
  ) where

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Throw (class MonadThrow)

import Data.Argonaut as J
import Data.Array as Array
import Data.Lens ((^.))
import Data.Map as Map
import Data.Path.Pathy as Path
import Data.String as String
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Prelude
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as PTM

import Utils.Path (FilePath)

eval
  ∷ ∀ m
  . ( MonadAsk CEM.CardEnv m
    , MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ PTM.Model
  → Port.DataMap
  → Port.Resource
  → m Port.Out
eval options varMap resource = do
  let
    filePath = resource ^. Port._filePath
    query = mkSql options filePath
  r ← CEM.temporaryOutputResource
  state ← get
  axes ←
    case state of
      Just (CEM.Analysis { axes: ax, resource: resource' })
        | resource' ≡ resource → pure ax
      _ → CEM.liftQ (QQ.axes filePath 300)
  let
    state' = { axes, records: [], resource }
    view = Port.View r (snd query) varMap
    output = Port.PivotTable (fst query) × SM.singleton Port.defaultResourceVar (Left view)
    backendPath = Left $ fromMaybe Path.rootDir (Path.parentDir r)
  put (Just (CEM.Analysis state'))
  when (Array.null options.columns) do
    CEM.throw "Please select a column to display"
  CEM.liftQ $ QQ.viewQuery backendPath r (snd query) SM.empty
  pure output

mkSql ∷ PTM.Model → FilePath → Port.PivotTablePort × String
mkSql options resource =
  let
    isSimple = PTM.isSimple options
    port = genPort isSimple options
    dimLen = Array.length port.dimensions
    groupBy = map (\(_ × value) → "row" <> CEC.escapeCursor value) port.dimensions
    dims =
      Array.mapWithIndex
        (\i (n × value) → "row" <> CEC.escapeCursor value <> " AS " <> Port.escapeIdentifier n)
        port.dimensions
    cols =
      Array.mapWithIndex
        case _, _ of
          i, n × PTM.Column c | isSimple → "row" <> CEC.escapeCursor c.value <> " AS " <> Port.escapeIdentifier n
          i, n × PTM.Column c → sqlAggregation c.valueAggregation ("row" <> CEC.escapeCursor c.value) <> " AS " <> Port.escapeIdentifier n
          i, n × PTM.Count    → "COUNT(*) AS " <> Port.escapeIdentifier n
        port.columns
    head =
      [ "SELECT " <> String.joinWith ", " (dims <> cols)
      , "FROM {{path}} AS row"
      ]
    tail =
      [ "GROUP BY " <> String.joinWith ", " groupBy
      , "ORDER BY " <> String.joinWith ", " groupBy
      ]
    sql =
      QQ.templated resource $
        String.joinWith " "
          if dimLen == 0
            then head
            else head <> tail
  in
    port × sql

genPort ∷ Boolean → PTM.Model → Port.PivotTablePort
genPort isSimpleQuery model =
  toPort
    (foldl go { names: Map.empty, taken: Set.empty }
      (map Left model.dimensions <> map Right model.columns))
  where
  toPort res =
    let
      dimensions = flip Array.mapMaybe model.dimensions \j → (_ × j) <$> Map.lookup (Left j) res.names
      columns = flip Array.mapMaybe model.columns \j → (_ × j) <$> Map.lookup (Right j) res.names
    in { dimensions, columns, isSimpleQuery }

  go { names, taken } a
    | Map.member a names = { names, taken }
    | otherwise =
        let name = uniqueTag 1 taken (genName a)
        in
          { names: Map.insert a name names
          , taken: Set.insert name taken
          }

  genName = case _ of
    Left j → topName j
    Right PTM.Count → "count"
    Right (PTM.Column { value }) → topName value

  uniqueTag n taken a =
    let name = if n ≡ 1 then a else a <> "_" <> show n
    in if Set.member name taken then uniqueTag (n + 1) taken a else name

topName ∷ J.JCursor → String
topName = case _ of
  J.JField c J.JCursorTop → c
  J.JField c (J.JIndex i J.JCursorTop) → c <> "_" <> show i
  J.JIndex i J.JCursorTop → "_" <> show i
  J.JField _ cs → topName cs
  J.JIndex _ cs → topName cs
  J.JCursorTop → "value"

sqlAggregation ∷ Maybe Ag.Aggregation → String → String
sqlAggregation a b = case a of
  Just Ag.Minimum → "MIN(" <> b <> ")"
  Just Ag.Maximum → "MAX(" <> b <> ")"
  Just Ag.Average → "AVG(" <> b <> ")"
  Just Ag.Sum     → "SUM(" <> b <> ")"
  _ → "[" <> b <> " ...]"
