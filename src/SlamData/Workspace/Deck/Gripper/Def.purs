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

module SlamData.Workspace.Deck.Gripper.Def where

import SlamData.Prelude
-- | Dependent on deck width any gripper may provide an interaction to select any
-- | card in the deck. Each gripper is guaranteed to provide a single interaction
-- | to select one card in one direction (either previous or next) if that card
-- | is available.

data GripperDef = Previous Boolean | Next Boolean

derive instance eqGripperDef ∷ Eq GripperDef
