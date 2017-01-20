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

module SlamData.Workspace.Card.FormInput.Eval
  ( evalTextLike
  , evalLabeled
  , module SlamData.Workspace.Card.FormInput.Model
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Writer.Class (class MonadTell)
import Control.Monad.State (class MonadState, get, put)

import Data.Foldable as F
import Data.Lens ((^.), preview)
import Data.List as List
import Data.Map as Map
import Data.Path.Pathy as Path
import Data.Set as Set
import Data.StrMap as SM

import Quasar.Types (SQL)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Eval.Common (validateResources, escapeCursor)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as CES
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.FormInput.Model (Model(..))
import SlamData.Workspace.Card.FormInput.LabeledRenderer.Model as LR
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model as TLR
import SlamData.Workspace.Card.Setups.Semantics as Sem

eval
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , MonadState CEM.CardState m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ SQL
  → String
  → Port.VarMapValue
  → Port.Resource
  → m Port.Out
eval sql var val r = do
  resource ← CEM.temporaryOutputResource
  let
    backendPath =
      Left $ fromMaybe Path.rootDir (Path.parentDir (r ^. Port._filePath))

  { inputs } ←
    CEM.liftQ $ lmap (QE.prefixMessage "Error compiling query") <$>
      QQ.compile backendPath sql SM.empty

  validateResources inputs
  CEM.addSources inputs
  CEM.liftQ do
    QQ.viewQuery backendPath resource sql SM.empty
    QFS.messageIfFileNotFound resource "Requested collection doesn't exist"
  let
    var' =
      if var ≡ Port.defaultResourceVar
         then var <> "2"
         else var
    varMap =
      SM.fromFoldable
        [ Port.defaultResourceVar × Left (Port.View resource sql SM.empty)
        , var' × Right val
        ]
  pure (Port.ResourceKey Port.defaultResourceVar × varMap)

evalLabeled
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , MonadState CEM.CardState m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ LR.Model
  → Port.SetupLabeledFormInputPort
  → Port.Resource
  → m Port.Out
evalLabeled m p r = do
  cardState ← get
  let
    lastUsedResource = cardState >>= preview CES._LastUsedResource

    selected
      -- Reloading
      | lastUsedResource ≡ Nothing ∧ (not Set.isEmpty m.selected) =
          m.selected
      -- same resource: take selected from model
      | lastUsedResource ≡ Just r =
          m.selected
      -- new resource and checkbox: empty selection
      | p.formInputType ≡ FIT.Checkbox =
          Set.empty
      -- default selection is empty, new resource this is not checkbox: take first value
      | Set.isEmpty p.selectedValues =
          foldMap Set.singleton $ List.head $ Map.keys p.valueLabelMap
      -- new resource, not checkbox: take default selection
      | otherwise =
          p.selectedValues

    semantics =
      foldMap Sem.semanticsToSQLStrings selected

    selection =
      "(" <> (F.intercalate "," semantics) <> ")"

    prettySelection =
      case semantics of
        [ a ] → a
        _     → selection

    sql =
      "SELECT * FROM `"
      <> Path.printPath (r ^. Port._filePath)
      <> "` AS res"
      <> " WHERE res"
      <> (escapeCursor p.cursor)
      <> " IN "
      <> selection
  when (lastUsedResource ≠ Just r)
    $ put $ Just $ CEM.AutoSelect {lastUsedResource: r, autoSelect: selected}

  eval sql p.name (Port.QueryExpr prettySelection) r

evalTextLike
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , MonadState CEM.CardState m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ TLR.Model
  → Port.SetupTextLikeFormInputPort
  → Port.Resource
  → m Port.Out
evalTextLike m p r = do
  cardState ← get
  let
    lastUsedResource = cardState >>= preview CES._LastUsedResource

    selection
      | lastUsedResource ≠ Just r = ""
      | p.formInputType ≡ FIT.Text = show m.value
      | otherwise = m.value

    sql =
      "SELECT * FROM `"
      <> Path.printPath (r ^. Port._filePath)
      <> "` AS res"
      <> " WHERE res"
      <> (escapeCursor p.cursor)
      <> " = "
      <> selection
  when (lastUsedResource ≠ Just r)
    $ put
    $ Just
    $ CEM.AutoSelect {lastUsedResource: r, autoSelect: (Set.empty ∷ Set.Set Sem.Semantics)}

  eval sql p.name (Port.QueryExpr selection) r
