module Test.SlamData.Feature.Effects where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Random (RANDOM)
import Node.ChildProcess (CHILD_PROCESS)
import Test.SlamData.Feature.Env (ENV)

type SlamFeatureEffects e =
  ( cp :: CHILD_PROCESS
  , avar :: AVAR
  , random :: RANDOM
  , env :: ENV
  | e)
