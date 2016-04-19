{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook.Card.Markdown.Component.State
  ( State
  , StateP
  , initialState

  , formStateToVarMap
  ) where



import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff)
import Data.Date.Locale as DL
import Data.StrMap as SM

import Halogen (ParentState)

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component as SDH

import SlamData.Notebook.Card.Markdown.Interpret as MDI
import SlamData.Notebook.Card.Port.VarMap as VM
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Effects (Slam)

type State = Maybe (SD.SlamDownP VM.VarMapValue)

initialState :: State
initialState = Nothing

type StateP =
  ParentState
    State (SDH.SlamDownState VM.VarMapValue)
    CardEvalQuery (SDH.SlamDownQuery VM.VarMapValue)
    Slam Unit

formStateToVarMap
  ∷ ∀ m e
  . (MonadEff (locale ∷ DL.Locale | e) m, Applicative m)
  ⇒ SDH.SlamDownFormDesc VM.VarMapValue
  → SDH.SlamDownFormState VM.VarMapValue
  → m VM.VarMap
formStateToVarMap desc st =
  SM.foldM
    (\m k field → do
       v ← valueForKey k field
       pure $ SM.insert k v m)
    SM.empty
    desc

  where
    valueForKey
      ∷ ∀ f a
      . String
      → SD.FormFieldP f a
      → m VM.VarMapValue
    valueForKey k field =
      fromMaybe (MDI.formFieldEmptyValue field) <$>
        case SM.lookup k st of
          Just v → MDI.formFieldValueToVarMapValue v
          Nothing → pure Nothing
