module SlamData.Dialog.Share.RotarySelector.Component.Query where

import DOM.HTML.Types as Ht

data Query a
  = Init Ht.HTMLElement a
  | StartDragging Number a
  | StopDragging a
  | Animated a
  | ChangePosition Number a
  | GetSelected (String -> a)
  | Selected String a
