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

module SlamData.Workspace.Deck.Gripper
  ( gripperDefsForCard
  , renderGrippers
  , module SlamData.Workspace.Deck.Gripper.Def
  ) where

import SlamData.Prelude

import Data.Array as Array
import Data.Bifoldable (bifoldMap)

import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Render.CSS as ClassNames
import SlamData.Workspace.Deck.Common (DeckHTML)
import SlamData.Workspace.Deck.Component.Query (Query(StartSliding))
import SlamData.Workspace.Deck.Component.State (DisplayCard, eqDisplayCard)
import SlamData.Workspace.Deck.Gripper.Def (GripperDef(..))

gripperDefsForCard
  ∷ Array DisplayCard
  → DisplayCard
  → GripperDef × GripperDef
gripperDefsForCard cards card =
  Previous previousCardAvailable × Next nextCardAvailable
  where
  previousCardAvailable =
    fromMaybe true $
      not ∘ eqDisplayCard card <$> Array.head cards

  nextCardAvailable =
    isRight card

isAvailable ∷ GripperDef → Boolean
isAvailable = case _ of
  Previous available → available
  Next available → available

gripperLabel ∷ GripperDef → String
gripperLabel = case _ of
  Previous _ → "Access previous card"
  Next _ → "Access next card"

gripperClassName ∷ GripperDef → ClassName
gripperClassName = case _ of
  Previous _ → ClassNames.cardGripper
  Next _ → ClassNames.cardGripperLast

renderGrippers ∷ Boolean → Boolean → GripperDef × GripperDef → Array DeckHTML
renderGrippers isActiveCard isGrabbed =
  bifoldMap renderSingleton renderSingleton
  where
  render ∷ GripperDef → DeckHTML
  render gripperDef =
    HH.button
      ([ HP.classes [ gripperClassName gripperDef ]
       , HE.onMouseDown $ HE.input (StartSliding gripperDef)
       , ARIA.grabbed $ show $ isGrabbed
       , ARIA.disabled $ show $ (not $ isAvailable gripperDef) || (not $ isActiveCard)
       ]
       ⊕ (guard (isActiveCard) $> ARIA.label (gripperLabel gripperDef))
      )
      []

  renderSingleton ∷ GripperDef → Array DeckHTML
  renderSingleton = Array.singleton ∘ render
