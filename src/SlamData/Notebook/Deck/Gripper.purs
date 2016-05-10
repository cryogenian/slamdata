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

module SlamData.Notebook.Deck.Gripper
  ( GripperDef
  , gripperDefsForCardId
  , renderGrippers
  ) where

import Data.Array as Array
import Data.List (List)
import Data.List as List
import Halogen as H
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Elements.Indexed as HH
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import SlamData.Notebook.Card.CardId (CardId)
import SlamData.Notebook.Deck.Common (NotebookHTML)
import SlamData.Notebook.Deck.Component.Query (Query(StartSliding))
import SlamData.Notebook.Deck.Component.State (CardDef)
import SlamData.Prelude
import SlamData.Render.CSS as ClassNames
import Data.Bifoldable (bifoldMap)

-- | Dependent on deck width any gripper may provide an interaction to select any
-- | card in the deck. Each gripper is guaranteed to provide a single interaction
-- | to select one card in one direction (either previous or next) if that card
-- | is available.

data GripperDef = Previous Boolean | Next Boolean

gripperDefsForCardId :: List CardDef -> Maybe CardId -> Tuple GripperDef GripperDef
gripperDefsForCardId cards cardId =
  Tuple (Previous previousCardAvailable) (Next nextCardAvailable)
  where
  previousCardAvailable =
    cardId /= (_.id <$> List.head cards)
  nextCardAvailable =
    isJust cardId

isAvailable :: GripperDef -> Boolean
isAvailable (Previous available) =
  available
isAvailable (Next available) =
  available

gripperLabel :: GripperDef -> String
gripperLabel (Previous _) =
  "Drag right to access previous cards"
gripperLabel (Next _) =
  "Drag left to access next cards"

gripperClassName :: GripperDef -> ClassName
gripperClassName (Previous _) = ClassNames.cardGripper
gripperClassName (Next _) = ClassNames.cardGripperLast

renderGrippers :: Boolean -> Boolean -> Tuple GripperDef GripperDef -> Array NotebookHTML
renderGrippers isActiveCard isGrabbed =
  bifoldMap renderSingleton renderSingleton
  where
  render :: GripperDef -> NotebookHTML
  render gripperDef =
    HH.div
      ([ HP.classes [ gripperClassName gripperDef ]
       , HE.onMouseDown \e -> HEH.preventDefault $> H.action (StartSliding e)
       , ARIA.grabbed $ show $ isGrabbed
       , ARIA.disabled $ show $ (not $ isAvailable gripperDef) || (not $ isActiveCard)
       ]
       ⊕ (guard (isActiveCard) $> ARIA.label (gripperLabel gripperDef))
      )
      []

  renderSingleton :: GripperDef -> Array NotebookHTML
  renderSingleton = Array.singleton <<< render
