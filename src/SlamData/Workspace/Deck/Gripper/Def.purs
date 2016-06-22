module SlamData.Workspace.Deck.Gripper.Def where

import SlamData.Prelude
-- | Dependent on deck width any gripper may provide an interaction to select any
-- | card in the deck. Each gripper is guaranteed to provide a single interaction
-- | to select one card in one direction (either previous or next) if that card
-- | is available.

data GripperDef = Previous Boolean | Next Boolean

derive instance eqGripperDef ∷ Eq GripperDef
