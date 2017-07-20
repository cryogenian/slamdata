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

module SlamData.Dialog.Message.Component where

import SlamData.Prelude

import Data.List.NonEmpty as NEL
import Data.Newtype (un)
import Halogen as H
import Halogen.Component.Proxy as Proxy
import Halogen.HTML as HH
import SlamData.Dialog.Component as Dialog

type MessageSpec o =
  { title ∷ String
  , message ∷ H.ComponentHTML (Const Void)
  , class_ ∷ H.ClassName
  , action ∷ Either String (String × o)
  }

mkSpec ∷ ∀ o. MessageSpec o → Dialog.DialogSpec o
mkSpec { title, class_, action, message } =
  { title
  , class_
  , dialog: Proxy.proxy (component message)
  , buttons: case action of
    Right (label × o) →
      dismissButton "Cancel"
        `NEL.cons` pure (Dialog.buttonPrimary label (Dialog.Confirm o))
    Left cancelLabel →
      pure (dismissButton cancelLabel)
  }
  where
    dismissButton ∷ String → Dialog.Button o
    dismissButton = flip Dialog.buttonDefault Dialog.Dismiss

component
  ∷ ∀ o m
  . H.ComponentHTML (Const Void)
  → H.Component HH.HTML (Const Void) Unit (Dialog.InnerMessage o) m
component h =
  H.component
    { initialState: const unit
    , render: const h
    , eval: absurd ∘ un Const
    , receiver: const Nothing
    }
