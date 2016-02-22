module SlamData.Dialog.Share.RotarySelector.Component.Query where

import DOM.HTML.Types as Ht
import SlamData.Dialog.Share.RotarySelector.Component.State

data Query a
  = Init Ht.HTMLElement a
  | StartDragging Number a
  | StopDragging a
  | Animated a
  | ChangePosition Number a
  | GetSelected (OptionR -> a)
  | Selected OptionR a
