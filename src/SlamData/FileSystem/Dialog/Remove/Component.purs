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

module SlamData.FileSystem.Dialog.Remove.Component
  ( Query(..)
  , component
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.FileSystem.Dialog.Component.Message (Message(..))
import SlamData.FileSystem.Resource as R
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import Utils.DOM as DOM

data Query a
  = PreventDefault DOM.Event a
  | Confirm a
  | Cancel a

component ∷ H.Component HH.HTML Query R.Resource Message Slam
component =
  H.component
    { initialState: id
    , receiver: const Nothing
    , render
    , eval
    }

render ∷ R.Resource → H.ComponentHTML Query
render res =
  modalDialog
    [ modalHeader "Confirm deletion"
    , modalBody
      $ HH.div_
        [ HH.text "Are you sure you want delete "
        , HH.code_ [ HH.text (R.resourceName res) ]
        , HH.text " ?"
        ]
    , modalFooter
        [ HH.button
          [ HP.type_ HP.ButtonSubmit
          , HP.classes [ CN.btn, CN.btnPrimary ]
          , HE.onClick $ HE.input_ Confirm
          ]
          [ HH.text "Delete" ]
        , HH.button
          [ HP.type_ HP.ButtonButton
          , HP.classes
            [ CN.btn
            , CN.btnDefault
            ]
          , HE.onClick $ HE.input_ Cancel
          ]
          [ HH.text "Cancel" ]
        ]
    ]

eval ∷ Query ~> H.ComponentDSL R.Resource Query Message Slam
eval = case _ of
  PreventDefault ev next →
    H.liftEff (DOM.preventDefault ev) $> next
  Cancel next →
    H.raise Dismiss $> next
  Confirm next → do
    res ← H.get
    H.raise (ResourceDelete res) $> next
