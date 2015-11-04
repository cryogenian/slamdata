module Notebook.Autosave
       ( autoSaveSignal
       ) where
import Prelude

import Control.Monad.Aff (Aff(), later')
import Notebook.Component (NotebookQueryP())
import Notebook.Component.Query (NotebookQuery(..))
import Data.Functor.Coproduct (left)
import Halogen
import Notebook.Effects (NotebookRawEffects(), NotebookEffects())

autoSaveSignal :: Driver NotebookQueryP NotebookRawEffects -> Aff NotebookEffects Unit
autoSaveSignal driver =
  later' 1000 do
    driver $ left $ action $ Save
    autoSaveSignal driver
