module SlamData.FileSystem.Dialog.Permissions.Component.Query where

import DOM.HTML.Types (HTMLElement())

data Query a
  = Init a
  | ToConfirm a
  | BackToForm a
  | Share a
  | Dismiss a
  | InitZClipboard HTMLElement a
