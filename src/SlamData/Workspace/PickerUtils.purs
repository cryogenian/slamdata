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

module SlamData.Workspace.PickerUtils
  ( datePickerFormat
  , dateTimePickerFormat
  , timePickerFormat
  , config
  ) where

import SlamData.Prelude

import Data.Newtype (over)
import Halogen.Datepicker.Config as PickerConfig
import Halogen.Datepicker.Format.Date as DatePickerF
import Halogen.Datepicker.Format.DateTime as DateTimePickerF
import Halogen.Datepicker.Format.Time as TimePickerF
import Halogen.HTML.Core (ClassName(..))
import SlamData.Render.ClassName (formControl)

dateTimePickerFormat ∷ DateTimePickerF.Format
dateTimePickerFormat = unsafePartial fromRight
  $ DateTimePickerF.fromString "YYYY-MMMM-DD HH:mm:ss"

datePickerFormat ∷ DatePickerF.Format
datePickerFormat = unsafePartial fromRight
  $ DatePickerF.fromString "YYYY-MMMM-DD"

timePickerFormat ∷ TimePickerF.Format
timePickerFormat = unsafePartial fromRight
  $ TimePickerF.fromString "HH:mm:ss"

config :: PickerConfig.Config
config = PickerConfig.defaultConfig
  <> over PickerConfig.Config
    (_{ input = [ClassName "Picker-input--large", formControl]
      , choice = [ClassName "Picker-input--large"]
      }) mempty
