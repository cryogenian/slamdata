module Utils.Variant where

import Prelude

import Control.Alternative (class Alternative, empty)

import Data.Array as Ar
import Data.Foldable as F
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple, fst)
import Data.Variant (Variant, case_)

import Type.Row (kind RowList, Nil, Cons, class RowToList)

import Unsafe.Coerce (unsafeCoerce)

downcast
  ∷ ∀ r rr out
  . Union r rr out
  ⇒ Variant r
  → Variant out
downcast = unsafeCoerce

case2_ ∷ ∀ a. Variant () → Variant () → a
case2_ _ = case_

data RWProxy (lt ∷ # Type) (gt ∷ # Type) = RWProxy
data RProxy (rl ∷ RowList) = RProxy

class LabelList (rl ∷ RowList) where
  list ∷ RProxy rl → Array String

instance nilList ∷ LabelList Nil where
  list _ = [ ]

instance consList ∷ (IsSymbol s, LabelList tail) ⇒ LabelList (Cons s a tail) where
  list _ = Ar.cons (reflectSymbol (SProxy ∷ SProxy s)) $ list (RProxy ∷ RProxy tail)

class Upcastable (gt ∷ # Type) (lt ∷ # Type) where
  labels ∷ RWProxy lt gt → Array String

instance upcastable ∷ (RowToList lt ltl, LabelList ltl, Union lt a gt) ⇒ Upcastable gt lt where
  labels _ = list (RProxy ∷ RProxy ltl)

upcast
  ∷ ∀ lt gt f
  . Upcastable gt lt
  ⇒ Alternative f
  ⇒ Variant gt
  → f (Variant lt)
upcast v =
  let
    keys = labels (RWProxy ∷ RWProxy lt gt)
    tpl ∷ ∀ ω. Tuple String ω
    tpl = unsafeCoerce v
    contains = F.elem (fst tpl) keys
  in if contains
     then pure $ unsafeCoerce v
     else empty
