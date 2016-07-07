module SlamData.Workspace.Deck.AdditionalSource where

import SlamData.Prelude

import Data.Path.Pathy as Pt

import Utils.Path (FilePath)

data AdditionalSource
  = Cache FilePath
  | Source FilePath

instance eqAdditionalSource ∷ Eq AdditionalSource where
  eq (Cache a) (Cache b) = Pt.printPath a ≡ Pt.printPath b
  eq (Source a) (Source b) = Pt.printPath a ≡ Pt.printPath b
  eq _ _ = false


instance ordAdditionalSource ∷ Ord AdditionalSource where
  compare (Cache a) (Cache b) = compare (Pt.printPath a) (Pt.printPath b)
  compare (Source a) (Source b) = compare (Pt.printPath a) (Pt.printPath b)
  compare (Cache _) _ = LT
  compare _ (Cache _) = GT
