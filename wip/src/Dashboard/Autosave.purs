{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

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
