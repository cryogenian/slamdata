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
  , formatDate
  , formatDateTime
  , formatTime
  , unformatDate
  , unformatDateTime
  , unformatTime
  ) where

import SlamData.Prelude

import Data.DateTime as DT
import Data.Either (hush)
import Data.Formatter.DateTime as FD
import Halogen.Datepicker.Format.Date as DatePickerF
import Halogen.Datepicker.Format.DateTime as DateTimePickerF
import Halogen.Datepicker.Format.Time as TimePickerF

dateTimePickerFormat ∷ DateTimePickerF.Format
dateTimePickerFormat = unsafePartial fromRight
  $ DateTimePickerF.fromString "YYYY-MMMM-DD HH:mm:ss"

datePickerFormat ∷ DatePickerF.Format
datePickerFormat = unsafePartial fromRight
  $ DatePickerF.fromString "YYYY-MMMM-DD"

timePickerFormat ∷ TimePickerF.Format
timePickerFormat = unsafePartial fromRight
  $ TimePickerF.fromString "HH:mm:ss"

dateTimeStringFormat ∷ String
dateTimeStringFormat = dateStringFormat' <> "T" <> timeStringFormat'

dateStringFormat ∷ String
dateStringFormat = dateStringFormat' <> "T" <> timeStringFormat' <> "Z"

timeStringFormat ∷ String
timeStringFormat = timeStringFormat' <> ".SSS"

dateStringFormat' ∷ String
dateStringFormat' = "YYYY-MM-DD"

timeStringFormat' ∷ String
timeStringFormat' = "HH:mm:ss"


formatDateTime ∷ DT.DateTime → Maybe String
formatDateTime x = hush $ FD.formatDateTime dateTimeStringFormat x

formatDate ∷ DT.Date → Maybe String
formatDate x = hush $ FD.formatDateTime dateStringFormat $ DT.DateTime x bottom

formatTime ∷ DT.Time → Maybe String
formatTime x = hush $ FD.formatDateTime timeStringFormat $ DT.DateTime bottom x

unformatDateTime ∷ String → Maybe DT.DateTime
unformatDateTime x = hush $ FD.unformatDateTime dateTimeStringFormat x

unformatDate ∷ String → Maybe DT.Date
unformatDate x = hush $ map DT.date $ FD.unformatDateTime dateStringFormat x

unformatTime ∷ String → Maybe DT.Time
unformatTime x = hush $ map DT.time $ FD.unformatDateTime timeStringFormat x
