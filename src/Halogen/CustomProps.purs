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

module Halogen.CustomProps
  ( MbIEventProp
  , InputProp
  , mbInput
  , mbValueInput
  , nonSubmit
  , mbClick
  , mbDoubleClick
  , mbMouseDown
  , mbKeyDown
  , mbKeyPress
  , onPaste
  , frameBorder
  ) where

import SlamData.Prelude

import Data.ExistsR (mkExistsR)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class IsForeign, readProp)

import Halogen.HTML.Core as H
import Halogen.HTML.Events.Handler (EventHandler, preventDefault)
import Halogen.HTML.Events.Indexed (IEventProp)
import Halogen.HTML.Events (EventProp)
import Halogen.HTML.Events.Types (Event, MouseEvent, KeyboardEvent)
import Halogen.HTML.Properties.Indexed (IProp, I, GlobalProperties, InteractiveEvents)

import Unsafe.Coerce (unsafeCoerce)

type MbEventProp e i = (Event e → EventHandler (Maybe i)) → H.Prop i

mbHandler ∷ ∀ fields i. H.EventName fields → MbEventProp fields i
mbHandler name k = H.Handler (mkExistsR (H.HandlerF name k))

addForeignMbHandler
  ∷ ∀ i value
  . (IsForeign value)
  ⇒ String → String → (value → EventHandler (Maybe i)) → H.Prop i
addForeignMbHandler key prop handler =
  H.handler'
  (H.eventName key)
  (either (const $ pure Nothing) handler ∘ readProp prop ∘ toForeign ∘ _.target)

type MbIEventProp r e i = (Event e → EventHandler (Maybe i)) → IProp r i

-- Forms

type InputProp f = IProp (InteractiveEvents (GlobalProperties (accept ∷ I, autocomplete ∷ I, autofocus ∷ I, checked ∷ I, disabled ∷ I, form ∷ I, formaction ∷ I, formenctype ∷ I, formmethod ∷ I, formnovalidate ∷ I, formtarget ∷ I, height ∷ I, list ∷ I, max ∷ I, min ∷ I, multiple ∷ I, onAbort ∷ I, onChange ∷ I, onError ∷ I, onInput ∷ I, onInvalid ∷ I, onLoad ∷ I, onSearch ∷ I, onSelect ∷ I, pattern ∷ I, placeholder ∷ I, readonly ∷ I, required ∷ I, size ∷ I, src ∷ I, step ∷ I, inputType ∷ I, value ∷ I, width ∷ I))) (f Unit)

mbInput ∷ ∀ r i. MbIEventProp (onInput ∷ I | r) () i
mbInput = unsafeCoerce unrefined
  where
  unrefined ∷ MbEventProp () i
  unrefined = mbHandler (H.eventName "input")

mbValueInput
  ∷ ∀ i r
   . (String → EventHandler (Maybe i))
  → IProp (value ∷ I, onInput ∷ I | r) i
mbValueInput = unsafeCoerce unrefined
  where
  unrefined ∷ (String → EventHandler (Maybe i)) → H.Prop i
  unrefined = addForeignMbHandler "input" "value"

nonSubmit ∷ ∀ i r . IProp (onSubmit ∷ I | r) i
nonSubmit = unsafeCoerce unrefined
  where
  unrefined ∷ H.Prop i
  unrefined = mbHandler (H.eventName "submit") (\_ → preventDefault $> Nothing)

-- Mouse events

mbClick ∷ ∀ r i. MbIEventProp (onClick ∷ I|r) MouseEvent i
mbClick = unsafeCoerce unrefined
  where
  unrefined ∷ MbEventProp MouseEvent i
  unrefined = mbHandler (H.eventName "click")

mbDoubleClick ∷ ∀ r i. MbIEventProp (onDoubleClick ∷ I|r) MouseEvent i
mbDoubleClick = unsafeCoerce unrefined
  where
  unrefined ∷ MbEventProp MouseEvent i
  unrefined = mbHandler (H.eventName "dblclick")

mbMouseDown ∷ ∀ r i. MbIEventProp (onMouseDown ∷ I|r) MouseEvent i
mbMouseDown = unsafeCoerce unrefined
  where
  unrefined ∷ MbEventProp MouseEvent i
  unrefined = mbHandler (H.eventName "mousedown")

-- Keyboard events

mbKeyDown ∷ ∀ r i. MbIEventProp (onKeyDown ∷ I|r) KeyboardEvent i
mbKeyDown = unsafeCoerce unrefined
  where
  unrefined ∷ MbEventProp KeyboardEvent i
  unrefined = mbHandler (H.eventName "keydown")

mbKeyPress ∷ ∀ r i. MbIEventProp (onKeyPress ∷ I|r) KeyboardEvent i
mbKeyPress = unsafeCoerce unrefined
  where
  unrefined ∷ MbEventProp KeyboardEvent i
  unrefined = mbHandler (H.eventName "keypress")

-- Non-standard

onPaste ∷ ∀ r i. IEventProp r () i
onPaste = unsafeCoerce unrefined
  where
  unrefined ∷ EventProp () i
  unrefined = H.handler (H.eventName "paste")

frameBorder ∷ ∀ r i. Int → IProp r i
frameBorder = unsafeCoerce unrefined
  where
  unrefined ∷ Int → H.Prop i
  unrefined = H.Attr Nothing (H.attrName "frameBorder") ∘ show
