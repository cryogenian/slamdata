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

module SlamData.Workspace.Card.Next.Component.State where

import SlamData.Prelude

import Data.Array as A
import Data.Lens (LensP, lens)

import Halogen.HTML as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.CardType (CardType, insertableCardTypes, cardName, lightCardGlyph)
import SlamData.Workspace.Card.CardType.ChartType (allChartTypes)

data NextAction
  = Insert CardType
  | Drill String String (Array NextAction)

instance eqNextAction ∷ Eq NextAction where
  eq (Insert cty1) (Insert cty2) = cty1 ≡ cty2
  eq (Drill n1 i1 ctys1) (Drill n2 i2 ctys2) =
    n1 ≡ n2
    ∧ i1 ≡ i2
    ∧ ctys1 ≡ ctys2
  eq _ _ = false

type State =
  { input ∷ Maybe Port
  , presentAddCardGuide ∷ Boolean
  , actions ∷ Array NextAction
  , activeActions ∷ Array NextAction
  , filterString ∷ String
  }

foldToArray ∷ NextAction → Array CardType
foldToArray (Insert cty) = [ cty ]
foldToArray (Drill _ _ as) = A.concat $ map foldToArray as

searchFilters ∷ NextAction → Array String
searchFilters (Insert cty) = [ cardName cty ]
searchFilters  (Drill name _ as) = [ name ] ⊕ A.concat (map searchFilters as)

actionLabel ∷ NextAction → String
actionLabel (Insert cty) = cardName cty
actionLabel (Drill name _ _) = name

actionGlyph ∷ ∀ s f. NextAction → H.HTML s f
actionGlyph (Insert cty) = lightCardGlyph cty
actionGlyph (Drill _ src _) = HH.img [ HP.src src ]

initialState ∷ State
initialState =
  { input: Nothing
  , presentAddCardGuide: true
  , actions: map Insert insertableCardTypes ⊕ [Drill "TROLOLO" "foo" [ ] ]
  , activeActions: map Insert insertableCardTypes ⊕ [Drill "TROLOLO" "foo" [ ] ]
  , filterString: ""
  }

_input ∷ ∀ a r. LensP { input ∷ a | r } a
_input = lens _.input (_ { input = _ })

_actions ∷ ∀ a r. LensP { actions ∷ a |r } a
_actions = lens _.actions (_ { actions = _ })

_activeActions ∷ ∀ a r. LensP { activeActions ∷ a | r} a
_activeActions = lens _.activeActions (_ { activeActions = _ })

_presentAddCardGuide ∷ ∀ a r. LensP { presentAddCardGuide ∷ a | r } a
_presentAddCardGuide = lens _.presentAddCardGuide (_ { presentAddCardGuide = _ })

_filterString ∷ ∀ a r. LensP { filterString ∷ a | r } a
_filterString = lens _.filterString (_ { filterString = _ })
