module Notebook.Common (Slam()) where

import Control.Monad.Aff (Aff())
import Notebook.Effects (NotebookEffects())

type Slam = Aff NotebookEffects
