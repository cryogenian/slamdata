module Notebook.Effects where

import Ace.Types (ACE())
import Halogen (HalogenEffects())
import Network.HTTP.Affjax (AJAX())

type NotebookEffects = HalogenEffects NotebookRawEffects

type NotebookRawEffects =
  ( ajax :: AJAX
  , ace :: ACE
  )
