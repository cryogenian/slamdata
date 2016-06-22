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

module SlamData.Workspace.Card.JTable.Component
  ( jtableComponent
  , module SlamData.Workspace.Card.JTable.Component.Query
  , module JTS
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class (throwError)

import Data.Argonaut.Core as JSON
import Data.Int as Int
import Data.Lens ((.~), (?~))

import Halogen as H

import SlamData.Effects (Slam)
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.JTable.Component.Query (QueryP, PageStep(..), Query(..))
import SlamData.Workspace.Card.JTable.Component.Render (render)
import SlamData.Workspace.Card.JTable.Component.State as JTS
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

jtableComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
jtableComponent =
  CC.makeCardComponent
    { cardType: CT.JTable
    , component: H.component { render, eval: evalCard ⨁ evalJTable }
    , initialState: JTS.initialState
    , _State: CC._JTableState
    , _Query: CC.makeQueryPrism CC._JTableQuery
    }

-- | Evaluates generic card queries.
evalCard ∷ CC.CardEvalQuery ~> (H.ComponentDSL JTS.State QueryP Slam)
evalCard = case _ of
  CC.EvalCard info output next → do
    for_ info.input $ CEQ.runCardEvalT_ ∘ runTable
    pure next
  CC.Save k → pure ∘ k =<< H.gets (Card.JTable ∘ JTS.toModel)
  CC.Load card next → do
    case card of
      Card.JTable model → H.set $ JTS.fromModel model
      _ → pure unit
    pure next
  CC.SetDimensions dims next → do
    H.modify
      $ JTS._levelOfDetails
      .~ if dims.width < 336.0 ∨ dims.height < 240.0
           then Low
           else High
    pure next
  CC.ModelUpdated _ next →
    pure next

runTable
  ∷ Port.Port
  → CC.CardEvalT (H.ComponentDSL JTS.State QueryP Slam) Unit
runTable =
  case _ of
    Port.TaggedResource trp → updateTable trp
    _ → throwError "Expected a TaggedResource input"

updateTable
  ∷ Port.TaggedResourcePort
  → CC.CardEvalT (H.ComponentDSL JTS.State QueryP Slam) Unit
updateTable { resource, tag } = do
  oldInput ← lift $ H.gets _.input
  when (((oldInput <#> _.resource) ≠ pure resource) || ((oldInput >>= _.tag) ≠ tag))
    $ lift $ resetState

  size ←
    lift (Quasar.count resource)
      >>= either (throwError ∘ Exn.message) pure

  lift $ H.modify $ JTS._input ?~ { resource, size, tag }
  p ← lift $ H.gets JTS.pendingPageInfo

  items ←
    lift (Quasar.sample resource ((p.page - 1) * p.pageSize) p.pageSize)
      >>= either (throwError ∘ Exn.message) pure

  lift $
    H.modify
      $ (JTS._isEnteringPageSize .~ false)
      ∘ (JTS._result ?~
           { json: JSON.fromArray items
           , page: p.page
           , pageSize: p.pageSize
           })

-- | Resets the state while preserving settings like page size.
resetState ∷ H.ComponentDSL JTS.State QueryP Slam Unit
resetState = H.modify (JTS._result .~ Nothing)

-- | Evaluates jtable-specific card queries.
evalJTable ∷ Natural Query (H.ComponentDSL JTS.State QueryP Slam)
evalJTable = case _ of
  StepPage step next →
    H.modify (JTS.stepPage step) $> next
  ChangePageSize pageSize next →
    for_ (Int.fromString pageSize) (H.modify ∘ JTS.resizePage) $> next
  StartEnterCustomPageSize next →
    H.modify (JTS._isEnteringPageSize .~ true) $> next
  SetCustomPageSize size next →
    H.modify (JTS.setPageSize size) $> next
  SetCustomPage page next →
    H.modify (JTS.setPage page) $> next
  Update next → do
    input ← H.gets _.input
    for_ input \{ resource, tag } →
      CEQ.runCardEvalT_ $ updateTable { resource, tag }
    CC.raiseUpdatedC' CC.StateOnlyUpdate
    pure next
