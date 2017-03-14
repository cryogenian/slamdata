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

module SlamData.Workspace.Card.Setups.ActionSelect.Option where

import SlamData.Prelude
import Halogen as H
import Halogen.HTML as HH
import Unsafe.Coerce (unsafeCoerce)

type OptionComponent f s m = H.Component HH.HTML f (Maybe s) (Maybe s) m

newtype Option s m = Option (OptionComponent (Const Void) s m)

derive instance newtypeOption ∷ Newtype (Option s m) _

option ∷ ∀ f s m. OptionComponent f s m → Option s m
option = Option ∘ unsafeCoerce
