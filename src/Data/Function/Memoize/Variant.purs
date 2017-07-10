module Data.Function.Memoize.Variant where

import SlamData.Prelude

import Control.Monad.Eff (runPure)
import Control.Monad.ST (runST)

import Data.StrMap.ST as ST

memoize ∷ ∀ a b. (a → String) → (a → b) → a → b
memoize hash f a = runPure $ runST do
  mp ← ST.new
  let h = hash a
  v ← ST.peek mp h
  case v of
    Just r → pure r
    Nothing → do
      let res = f a
      _ ← ST.poke mp h res
      pure res
