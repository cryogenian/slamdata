module SlamData.FileSystem.Dialog.Permissions.Component.Query where

import SlamData.Prelude
import DOM.HTML.Types (HTMLElement())

data Query a
  = Init a
  | ToConfirm a
  | BackToForm a
  | Share a
  | Dismiss a
  | InitZClipboard (Maybe HTMLElement) a
