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

module SlamData.Workspace.FormBuilder.Item.Component.State
  ( State
  , initialState
  , putModel
  , getModel
  , module SlamData.Workspace.FormBuilder.Item.Model
  ) where

import SlamData.Workspace.FormBuilder.Item.Model

import Data.BrowserFeatures.InputType (InputType)
import Data.Maybe (Maybe)
import Text.Markdown.SlamDown.Halogen.Component (defaultBrowserFeatures)

type State =
  { name ∷ FieldName
  , fieldType ∷ FieldType
  , defaultValue ∷ Maybe String
  , inputTypeSupported ∷ InputType → Boolean
  }

putModel :: Model → State → State
putModel m s = s
  { name = m.name
  , fieldType = m.fieldType
  , defaultValue = m.defaultValue
  }

getModel :: State → Model
getModel s =
  { name: s.name
  , fieldType: s.fieldType
  , defaultValue: s.defaultValue
  }

initialState ∷ State
initialState =
  { name: initialModel.name
  , fieldType: initialModel.fieldType
  , defaultValue: initialModel.defaultValue
  , inputTypeSupported: defaultBrowserFeatures.inputTypeSupported
  }
