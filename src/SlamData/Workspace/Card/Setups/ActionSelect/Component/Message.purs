module SlamData.Workspace.Card.Setups.ActionSelect.Component.Message where

import SlamData.Prelude

data Message s
  = Dismiss
  | Confirm (Maybe s)
