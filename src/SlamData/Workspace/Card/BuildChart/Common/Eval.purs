module SlamData.Workspace.Card.BuildChart.Common.Eval
  ( records
  , type (>>)
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray)
import Data.Map as M

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.CardEvalT as CET

infixr 3 type M.Map as >>

records
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ FilePath
  → CET.CardEvalT m JArray
records resource = do
  numRecords ←
    CET.liftQ $ QQ.count resource

  when (numRecords > 10000)
    $ QE.throw
    $ "The 10000 record limit for visualizations has been exceeded - the current dataset contains "
    ⊕ show numRecords
    ⊕ " records. "
    ⊕ "Please consider using a 'limit' or 'group by' clause in the query to reduce the result size."

  CET.liftQ $ QQ.all resource
