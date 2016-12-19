module SlamData.Workspace.Card.SetupFormInput.Static.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.Static.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Throw (class MonadThrow)

import Data.Array as Arr

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.SetupFormInput.Static.Model (Model)

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.TaggedResourcePort
  → m Port.Port
eval m tr = do
  records × axes ← BCE.analyze tr =<< get
  put (Just (CEM.Analysis { taggedResource: tr, records, axes}))
  case m of
    Nothing → CEM.throw "Please select axis to show"
    Just conf → do
      eiSample ← QQ.sample tr.resource 0 1
      case eiSample of
        Left _ → CEM.throw "Empty or non-existent resource"
        Right record →
          case Arr.head record >>= flip Sem.getMaybeString conf.value of
            Nothing → CEM.throw $ show conf.value <> " axis is not presented in this resource"
            Just value →
              pure
                $ Port.Metric
                $ { value
                  , label: Nothing
                  , taggedResource: tr
                  }
