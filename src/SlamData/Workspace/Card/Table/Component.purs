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

module SlamData.Workspace.Card.Table.Component
  ( tableComponent
  , module SlamData.Workspace.Card.Table.Component.Query
  , module JTS
  ) where

import SlamData.Prelude

import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET

import Data.Argonaut as JSON
import Data.Int as Int
import Data.Lens ((.~), (?~), (^.), (^?))
import Data.Array as Arr

import DOM.Classy.Event as DOM

import Halogen as H

import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Table.Component.Query (PageStep(..), Query(..))
import SlamData.Workspace.Card.Table.Component.Render (render)
import SlamData.Workspace.Card.Table.Component.State as JTS
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardDSL JTS.State Query

tableComponent ∷ CC.CardOptions → CC.CardComponent
tableComponent =
  CC.makeCardComponent CT.Table $ H.component
    { render
    , eval: evalCard ⨁ evalTable
    , initialState: const JTS.initialState
    , receiver: const Nothing
    }

-- | Evaluates generic card queries.
evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure ∘ k =<< H.gets (Card.Table ∘ JTS.toModel)
  CC.Load card next → do
    case card of
      Card.Table model → H.put $ JTS.fromModel model
      _ → pure unit
    pure next
  CC.ReceiveInput _ varMap next → do
    runTable varMap
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState s next → do
    for_ (s ^? ES._Table) \t → do
      traceAnyA t
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 360.0 ∨ dims.height < 240.0
      then Low
      else High

runTable
  ∷ Port.DataMap
  → DSL Unit
runTable varMap = case Port.extractResource varMap of
  Just resource → updateTable resource
  _ → H.modify $ JTS._result .~ JTS.Errored "Expected a TaggedResource input"

updateTable
  ∷ Port.Resource
  → DSL Unit
updateTable resource = do
  r ← ET.runExceptT do
    oldInput ← lift $ H.gets _.input
    when ((oldInput <#> _.resource) ≠ pure resource)
      $ lift $ resetState

    size ← liftQ $ Quasar.count $ resource ^. Port._filePath
    lift $ H.modify $ JTS._input ?~ { resource, size }
    p ← lift $ H.gets JTS.pendingPageInfo

    items ←
      liftQ $ Quasar.sample (resource ^. Port._filePath) ((p.page - 1) * p.pageSize) p.pageSize

    let
      result
        | Arr.null items = JTS.Empty
        | otherwise = JTS.Ready
            { json: JSON.fromArray items
            , page: p.page
            , pageSize: p.pageSize
            }

    pure result
  H.modify case r of
    Left s → JTS._result .~ JTS.Errored (QE.printQError s)
    Right result → (JTS._result .~ result) ∘ (JTS._isEnteringPageSize .~ false)


liftQ
  ∷ ∀ m a
  . (EC.MonadError QE.QError m)
  ⇒ m (Either QE.QError a)
  → m a
liftQ m = either EC.throwError pure =<< m

-- | Resets the state while preserving settings like page size.
resetState ∷ DSL Unit
resetState = H.modify (JTS._result .~ JTS.Loading)

-- | Evaluates table-specific card queries.
evalTable ∷ Query ~> DSL
evalTable = case _ of
  StepPage step next → do
    H.modify (JTS.stepPage step)
    refresh
    pure next
  ChangePageSize pageSize next → do
    for_ (Int.fromString pageSize) (H.modify ∘ JTS.resizePage)
    refresh
    pure next
  StartEnterCustomPageSize next →
    H.modify (JTS._isEnteringPageSize .~ true) $> next
  SetCustomPageSize size next →
    H.modify (JTS.setPageSize size) $> next
  SetCustomPage page next →
    H.modify (JTS.setPage page) $> next
  Update next →
    refresh $> next
  PreventDefault ev next → do
    H.liftEff $ DOM.preventDefault ev
    pure next

refresh ∷ DSL Unit
refresh = do
  input ← H.gets _.input
  for_ input $ _.resource ⋙ updateTable
  H.raise $ CC.modelUpdate
