module SlamData.Workspace.LevelOfDetails where

import SlamData.Prelude

data LevelOfDetails
  = Low
  | High

derive instance eqLevelOfDetails ∷ Eq LevelOfDetails
derive instance ordLevelOfDetails ∷ Ord LevelOfDetails
