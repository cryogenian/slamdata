module Input.Notebook where

import Model.Action (Action(..))
import Model.Notebook

updateState :: State -> Input -> State
updateState state input =
  case input of
    ViewNotebook id cs -> NotebookView View id cs
    EditNotebook id cs -> NotebookView Edit id cs
    ViewCell c -> OneCellView View c
    EditCell c -> OneCellView Edit c
