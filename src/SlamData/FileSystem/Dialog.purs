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

module SlamData.FileSystem.Dialog where

import SlamData.Prelude

import Halogen.HTML as HH
import SlamData.Dialog.Component (DialogSpec)
import SlamData.Dialog.Message.Component as Message
import SlamData.FileSystem.Resource as R

data Definition = Delete R.Resource
type Action = Definition

dialog ∷ Definition → DialogSpec Action
dialog = case _ of
  Delete res →
    Message.mkSpec
      { title: "Confirm deletion"
      , message:
        HH.div_
          [ HH.text "Are you sure you want delete "
          , HH.code_ [ HH.text (R.resourceName res) ]
          , HH.text " ?"
          ]
      , class_: HH.ClassName "sd-delete-dialog"
      , action: Right ("Delete" × Delete res)
      }
