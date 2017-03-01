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

module SlamData.Workspace.Card.Table.Eval
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, put, get)
import Control.Monad.Throw (class MonadThrow)

import Data.Lens ((^.), (^?), _Just)

import SlamData.Quasar.Query as Quasar
import SlamData.Quasar.Error as QE

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Table.Model as M

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ M.Model
  → Port.Port
  → Port.DataMap
  → m Port.Out
eval model port varMap =
  Port.extractResource varMap
    # maybe (CEM.throw "Expected a TaggedResource input") \resource → do
      rawTableState ← map (_ ^? _Just ∘ ES._Table ) get
      let
        defaultState =
          { resource
          , result: Right [ ]
          , size: 0
          , page: 1
          , pageSize: 10
          }
        state =
          fromMaybe defaultState rawTableState
        pageSize =
          fromMaybe state.pageSize model.pageSize
        page =
          fromMaybe state.page model.page

      unless (samePageAndResource rawTableState resource model) do
        resultAndSize ← runExceptT do
          size ←
            case rawTableState of
              Just t | t.resource ≡ resource → pure t.size
              _ → liftET $ Quasar.count $ resource ^. Port._filePath

          result ←
            liftET
            $ Quasar.sample
            (resource ^. Port._filePath)
            ((page - 1) * pageSize)
            pageSize
          pure $ result × size
        put $ Just $ ES.Table
          { resource
          , result: map fst resultAndSize
          , size: either zero snd resultAndSize
          , page
          , pageSize
          }
        either
          CEM.throw
          (const $ pure unit)
          resultAndSize

      pure $ port × varMap
  where
  samePageAndResource ∷ Maybe ES.TableR → Port.Resource → M.Model → Boolean
  samePageAndResource Nothing _ _ = false
  samePageAndResource (Just t) resource {page, pageSize} =
    t.resource ≡ resource
    ∧ Just t.page ≡ page
    ∧ Just t.pageSize ≡ pageSize

  liftET ∷ ∀ a. m (CEM.CardError ⊹ a) → ExceptT String m a
  liftET = ExceptT ∘ map (lmap QE.printQError)
