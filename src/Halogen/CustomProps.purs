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

module Halogen.CustomProps (InputProp, nonSubmit) where

import SlamData.Prelude

import Halogen.HTML.Core as H
import Halogen.HTML.Events.Handler (preventDefault)
import Halogen.HTML.Properties.Indexed (IProp, I, InteractiveEvents, GlobalProperties)

import Unsafe.Coerce (unsafeCoerce)

type InputProp f = IProp (InteractiveEvents (GlobalProperties (accept ∷ I, autocomplete ∷ I, autofocus ∷ I, checked ∷ I, disabled ∷ I, form ∷ I, formaction ∷ I, formenctype ∷ I, formmethod ∷ I, formnovalidate ∷ I, formtarget ∷ I, height ∷ I, list ∷ I, max ∷ I, min ∷ I, multiple ∷ I, onAbort ∷ I, onChange ∷ I, onError ∷ I, onInput ∷ I, onInvalid ∷ I, onLoad ∷ I, onSearch ∷ I, onSelect ∷ I, pattern ∷ I, placeholder ∷ I, readonly ∷ I, required ∷ I, size ∷ I, src ∷ I, step ∷ I, inputType ∷ I, value ∷ I, width ∷ I))) (f Unit)

nonSubmit ∷ ∀ i r . IProp (onSubmit ∷ I | r) i
nonSubmit = unsafeCoerce unrefined
  where
  unrefined ∷ H.Prop i
  unrefined = H.handler (H.eventName "submit") (\_ → preventDefault $> Nothing)
