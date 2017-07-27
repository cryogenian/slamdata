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
import Halogen.HTML.Properties as HP
import SlamData.Dialog.Component as D
import SlamData.Dialog.Message.Component as Message
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Dialog.Delete.Component as Delete
import SlamData.FileSystem.Dialog.Share.Component as Share
import SlamData.FileSystem.Resource as R
import SlamData.Render.ClassName as CN
import SlamData.Monad (Slam)
import Utils.Path as UP

type Query = D.Query Definition Action
type Message = D.Message Action

data Definition
  = Error String
  | Delete R.Resource
  | Download R.Resource
  | Mount Mount.Input
  | Rename R.Resource
  | Share { name ∷ String, url ∷ String }

type Action = Variant
  ( deleted ∷ UP.AnyPath
  , renamed ∷ Unit
  , mounted ∷ Unit
  )

dialog ∷ Definition → D.DialogSpec Action Slam
dialog = case _ of
  Error msg →
    Message.mkSpec
      { title: "Error"
      , message:
          HH.div
            [ HP.classes [ CN.alert, CN.alertDanger ] ]
            [ HH.text msg ]
      , class_: HH.ClassName "sd-error-dialog"
      , action: Left "Dismiss"
      }
  Delete res → Delete.dialog res
  Download res → Download.dialog res
  Mount input → Mount.dialog input
  Rename res → Rename.dialog res
  Share opts → Share.dialog opts
