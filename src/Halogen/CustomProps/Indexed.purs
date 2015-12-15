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

module Halogen.CustomProps.Indexed where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

import Data.Maybe (Maybe())
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Types (Event())
import Halogen.HTML.Properties.Indexed (IProp(), I())
import Halogen.CustomProps as Cp

type MbIEventProp r e i = (Event e -> EventHandler (Maybe i)) -> IProp r i

mbInput :: forall r i. MbIEventProp (onInput :: I | r) () i
mbInput = unsafeCoerce Cp.mbInput

mbValueInput
  :: forall i r
   . (String -> EventHandler (Maybe i)) -> IProp (value :: I, onInput :: I | r) i
mbValueInput = unsafeCoerce Cp.mbValueInput

nonSubmit :: forall i r . IProp (onSubmit :: I | r) i
nonSubmit = unsafeCoerce Cp.nonSubmit

ariaLabel :: forall i r. String -> IProp (|r) i
ariaLabel = unsafeCoerce Cp.ariaLabel
