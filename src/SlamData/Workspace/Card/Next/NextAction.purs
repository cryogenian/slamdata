{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

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
