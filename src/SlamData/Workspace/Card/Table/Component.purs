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

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class (throwError)

import Data.Argonaut.Core as JSON
import Data.Int as Int
import Data.Lens ((.~), (?~))

import Halogen as H

import SlamData.Effects (Slam)
import SlamData.Quasar.Auth.Reauthentication (RequestIdTokenBus)
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Table.Component.Query (QueryP, PageStep(..), Query(..))
import SlamData.Workspace.Card.Table.Component.Render (render)
import SlamData.Workspace.Card.Table.Component.State as JTS
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = H.ComponentDSL JTS.State QueryP Slam

tableComponent ∷ RequestIdTokenBus → H.Component CC.CardStateP CC.CardQueryP Slam
tableComponent requestNewIdTokenBus =
  CC.makeCardComponent
    { cardType: CT.Table
    , component: H.component { render, eval: evalCard requestNewIdTokenBus ⨁ evalTable requestNewIdTokenBus }
    , initialState: JTS.initialState
    , _State: CC._TableState
    , _Query: CC.makeQueryPrism CC._TableQuery
    }

-- | Evaluates generic card queries.
evalCard ∷ RequestIdTokenBus → CC.CardEvalQuery ~> DSL
evalCard requestNewIdTokenBus = case _ of
  CC.EvalCard info output next → do
    for_ info.input $ CEQ.runCardEvalT_ ∘ runTable requestNewIdTokenBus
    pure next
  CC.Activate next →
    pure next
  CC.Save k →
    pure ∘ k =<< H.gets (Card.Table ∘ JTS.toModel)
  CC.Load card next → do
    case card of
      Card.Table model → H.set $ JTS.fromModel model
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
  CC.ZoomIn next →
    pure next

runTable
  ∷ RequestIdTokenBus
  → Port.Port
  → CC.CardEvalT (H.ComponentDSL JTS.State QueryP Slam) Unit
runTable requestNewIdTokenBus =
  case _ of
    Port.TaggedResource trp → updateTable requestNewIdTokenBus trp
    _ → throwError "Expected a TaggedResource input"

updateTable
  ∷ RequestIdTokenBus
  → Port.TaggedResourcePort
  → CC.CardEvalT (H.ComponentDSL JTS.State QueryP Slam) Unit
updateTable requestNewIdTokenBus { resource, tag } = do
  oldInput ← lift $ H.gets _.input
  when (((oldInput <#> _.resource) ≠ pure resource) || ((oldInput >>= _.tag) ≠ tag))
    $ lift $ resetState

  size ←
    lift (Quasar.count requestNewIdTokenBus resource)
      >>= either (throwError ∘ Exn.message) pure

  lift $ H.modify $ JTS._input ?~ { resource, size, tag }
  p ← lift $ H.gets JTS.pendingPageInfo

  items ←
    lift (Quasar.sample requestNewIdTokenBus resource ((p.page - 1) * p.pageSize) p.pageSize)
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
resetState ∷ DSL Unit
resetState = H.modify (JTS._result .~ Nothing)

-- | Evaluates table-specific card queries.
evalTable ∷ RequestIdTokenBus → Query ~> (H.ComponentDSL JTS.State QueryP Slam)
evalTable requestNewIdTokenBus = case _ of
  StepPage step next → do
    H.modify (JTS.stepPage step)
    refresh requestNewIdTokenBus
    pure next
  ChangePageSize pageSize next → do
    for_ (Int.fromString pageSize) (H.modify ∘ JTS.resizePage)
    refresh requestNewIdTokenBus
    pure next
  StartEnterCustomPageSize next →
    H.modify (JTS._isEnteringPageSize .~ true) $> next
  SetCustomPageSize size next →
    H.modify (JTS.setPageSize size) $> next
  SetCustomPage page next →
    H.modify (JTS.setPage page) $> next
  Update next →
    refresh requestNewIdTokenBus $> next

refresh ∷ RequestIdTokenBus → DSL Unit
refresh requestNewIdTokenBus = do
  input ← H.gets _.input
  for_ input \{ resource, tag } →
    CEQ.runCardEvalT_ $ updateTable requestNewIdTokenBus { resource, tag }
  CC.raiseUpdatedC' CC.StateOnlyUpdate
