module SlamData.Workspace.Card.Next.NextAction where

import SlamData.Prelude

import Data.Array as A

import Halogen.HTML as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.CardType (CardType, cardName, lightCardGlyph)
import SlamData.Render.Common (glyph)

data NextAction
  = Insert CardType
  | Drill String String (Array NextAction)
  | GoBack

instance eqNextAction ∷ Eq NextAction where
  eq GoBack GoBack = true
  eq (Insert cty1) (Insert cty2) = cty1 ≡ cty2
  eq (Drill n1 i1 ctys1) (Drill n2 i2 ctys2) =
    n1 ≡ n2
    ∧ i1 ≡ i2
    ∧ ctys1 ≡ ctys2
  eq _ _ = false


foldToArray ∷ NextAction → Array CardType
foldToArray (Insert cty) = [ cty ]
foldToArray (Drill _ _ as) = A.concat $ map foldToArray as
foldToArray (GoBack) = [ ]

searchFilters ∷ NextAction → Array String
searchFilters (Insert cty) = [ cardName cty ]
searchFilters (Drill name _ as) = [ name ] ⊕ A.concat (map searchFilters as)
searchFilters (GoBack) = [ ]

actionLabel ∷ NextAction → String
actionLabel (Insert cty) = cardName cty
actionLabel (Drill name _ _) = name
actionLabel (GoBack) = "Back"

actionGlyph ∷ ∀ s f. NextAction → H.HTML s f
actionGlyph (Insert cty) = lightCardGlyph cty
actionGlyph (Drill _ src _) = HH.img [ HP.src src ]
actionGlyph (GoBack) = glyph B.glyphiconChevronLeft
