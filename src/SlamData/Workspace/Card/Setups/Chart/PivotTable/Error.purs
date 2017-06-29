{-
Copyright 2017 SlamData, Inc.

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


module SlamData.Workspace.Card.Setups.Chart.PivotTable.Error where

import SlamData.Prelude

import Quasar.Advanced.QuasarAF (QError)
import SlamData.GlobalError as GE
import Utils (throwVariantError, hush)

data PivotTableError
  = PivotTableNoColumnSelectedError
  | PivotTableQuasarError QError

instance showPivotTableError ∷ Show PivotTableError where
  show = case _ of
    PivotTableNoColumnSelectedError → "PivotTableNoColumnSelectedError"
    PivotTableQuasarError qErr → "(PivotTableQuasarError " <> show qErr <> ")"

pivotTableToGlobalError ∷ PivotTableError → Maybe GE.GlobalError
pivotTableToGlobalError = case _ of
  PivotTableQuasarError qErr → hush (GE.fromQError qErr)
  _ → Nothing

throwPivotTableError ∷ forall v m a. MonadThrow (Variant (pivotTable ∷ PivotTableError | v)) m ⇒ PivotTableError → m a
throwPivotTableError = throwVariantError (SProxy :: SProxy "pivotTable")
