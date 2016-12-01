module SlamData.Workspace.Card.FormInput.Eval
  ( eval
  , module SlamData.Workspace.Card.FormInput.Model
  ) where

import SlamData.Prelude

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SLamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.CardEvalT as CET

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → CET.CardEvalT m Port.Port
eval Nothing =
  QE.throw "Incorrect port in form input card"
eval (Just m) =
  pure Port.Blocked
