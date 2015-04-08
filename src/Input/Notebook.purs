module Input.Notebook where

import qualified Model.Notebook as M
import Model.Resume

updateState :: M.State -> M.Input -> M.State
updateState state input =
  case input of
    M.ViewNotebook id cs -> M.NotebookView View id cs
    M.EditNotebook id cs -> M.NotebookView Edit id cs
    M.ViewCell c -> M.OneCellView View c
    M.EditCell c -> M.OneCellView Edit c


