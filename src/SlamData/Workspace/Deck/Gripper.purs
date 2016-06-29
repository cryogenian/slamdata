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

import Data.Array as Array
import Halogen as H
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Elements.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.CardId (CardId(CardId))
import SlamData.Workspace.Deck.Common (DeckHTML)
import SlamData.Workspace.Deck.Component.Query (Query(StartSliding))
import SlamData.Workspace.Deck.Component.State (coordModelToCoord)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Gripper.Def (GripperDef(..))
import SlamData.Prelude
import SlamData.Render.CSS as ClassNames
import Data.Bifoldable (bifoldMap)

gripperDefsForCard
  ∷ Array (DeckId × Card.Model)
  → Maybe (DeckId × CardId)
  → GripperDef × GripperDef
gripperDefsForCard cards coord =
  Previous previousCardAvailable × Next nextCardAvailable
  where
  previousCardAvailable =
    fromMaybe true
      $ not ∘ eq
      <$> coord
      <*> map coordModelToCoord (Array.head cards)

  nextCardAvailable = case coord of
    Just (_ × (CardId _)) → true
    _ → false

isAvailable ∷ GripperDef → Boolean
isAvailable (Previous available) =
  available
isAvailable (Next available) =
  available

gripperLabel ∷ GripperDef → String
gripperLabel (Previous _) =
  "Access previous card"
gripperLabel (Next _) =
  "Access next card"

gripperClassName ∷ GripperDef → ClassName
gripperClassName (Previous _) = ClassNames.cardGripper
gripperClassName (Next _) = ClassNames.cardGripperLast

renderGrippers ∷ Boolean → Boolean → GripperDef × GripperDef → Array DeckHTML
renderGrippers isActiveCard isGrabbed =
  bifoldMap renderSingleton renderSingleton
  where
  render ∷ GripperDef → DeckHTML
  render gripperDef =
    HH.button
      ([ HP.classes [ gripperClassName gripperDef ]
       , HE.onMouseDown \e →
             pure $ Just (H.action (StartSliding e gripperDef))
       , ARIA.grabbed $ show $ isGrabbed
       , ARIA.disabled $ show $ (not $ isAvailable gripperDef) || (not $ isActiveCard)
       ]
       ⊕ (guard (isActiveCard) $> ARIA.label (gripperLabel gripperDef))
      )
      []

  renderSingleton ∷ GripperDef → Array DeckHTML
  renderSingleton = Array.singleton ∘ render
