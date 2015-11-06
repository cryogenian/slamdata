module Dashboard.Autosave
       ( autoSaveSignal
       ) where
import Prelude

import Control.Monad.Aff (Aff(), later')
import Dashboard.Component (QueryP(), toDashboard, Query(..))
import Halogen (Driver())
import Notebook.Effects (NotebookRawEffects(), NotebookEffects())

autoSaveSignal :: Driver QueryP NotebookRawEffects -> Aff NotebookEffects Unit
autoSaveSignal driver =
  later' 1000 do
    driver $ toDashboard $ Save
    autoSaveSignal driver
