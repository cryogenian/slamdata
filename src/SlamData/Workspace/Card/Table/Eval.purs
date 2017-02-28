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

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Port.Port
  → Port.DataMap
  → m Port.Out
eval port varMap =
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
        state = fromMaybe defaultState rawTableState
      -- TODO: check if page/pageSize/resource hasn't been changed
      resultAndSize ← runExceptT do
        size ←
          ExceptT
          $ map (lmap QE.printQError)
          $ Quasar.count
          $ resource ^. Port._filePath
        result ←
          ExceptT
          $ map (lmap QE.printQError)
          $ Quasar.sample
          (resource ^. Port._filePath)
          ((state.page - 1) * state.pageSize)
          state.pageSize
        pure $ result × size

      put $ Just $ ES.Table
        { resource
        , result: map fst resultAndSize
        , size: either zero snd resultAndSize
        , page: state.page
        , pageSize: state.pageSize
        }
      either
        CEM.throw
        (const $ pure unit)
        resultAndSize

      pure $ port × varMap
