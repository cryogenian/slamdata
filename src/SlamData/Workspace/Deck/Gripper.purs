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

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Render.ClassName as CN
import SlamData.Render.Icon as I
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

gripperClassName ∷ GripperDef → ClassName
gripperClassName = case _ of
  Previous _ → CN.cardGripper
  Next _ → CN.cardGripperLast

gripperIcon ∷ ∀ p i. GripperDef → H.HTML p i
gripperIcon = case _ of
  Previous _ → I.gripperArrowLeft
  Next _ → I.gripperArrowRight

gripperLabel ∷ GripperDef → String
gripperLabel = case _ of
  Previous _ → "Access previous card"
  Next _ → "Access next card"

renderGrippers ∷ Boolean → Boolean → GripperDef × GripperDef → Array (String × DeckHTML)
renderGrippers isActiveCard isGrabbed =
  bifoldMap (render "previous") (render "next")
  where
  render ∷ String → GripperDef → Array (String × DeckHTML)
  render key gripperDef =
    if not isAvailable gripperDef || not isActiveCard then
      []
    else
      [ ("gripper-" <> key) × HH.button
          ([ HP.classes [ gripperClassName gripperDef ]
           , HP.title $ gripperLabel gripperDef
           , HE.onMouseDown $ HE.input $ StartSliding gripperDef
           , ARIA.grabbed $ show isGrabbed
           , ARIA.label $ gripperLabel gripperDef
           ]
          )
          [ gripperIcon gripperDef ]
      ]
