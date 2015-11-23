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
