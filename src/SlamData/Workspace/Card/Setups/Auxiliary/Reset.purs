module SlamData.Workspace.Card.Setups.Auxiliary.Reset where

import SlamData.Prelude

import Data.Functor.Variant (VariantF, FProxy)

import Halogen as H
import Halogen.HTML as HH

type ResetF m r = VariantF ( reset ∷ FProxy (Tuple m) | r)

type AuxComponent a b m = H.Component HH.HTML (ResetF b a) (Maybe b) b m
