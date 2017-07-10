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

module SlamData.AdminUI.Dialog.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.Types as QAT
import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN

data Dialog = ConfirmDeletion QAT.GroupPath

derive instance eqDialog ∷ Eq Dialog
derive instance ordDialog ∷ Ord Dialog

data Query a = Raise Message a

data Message
  = Confirm Dialog
  | Dismiss

component ∷ Dialog → H.Component HH.HTML Query Unit Message Slam
component dlg =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render ∷ Unit → H.ComponentHTML Query
  render _ =
    HH.div
      [ HP.classes [ CN.dialogContainer ]
      , HE.onClick (HE.input_ (Raise Dismiss))
      ]
      [ modalDialog
          [ modalHeader (headerMessage dlg)
          , modalBody (renderDialog dlg)
          , modalFooter
              [ HH.button
                  [ HP.classes [ CN.btn, CN.btnPrimary ]
                  , HE.onClick (HE.input_ (Raise (Confirm dlg)))
                  ]
                  [ HH.text (confirmMessage dlg) ]
              , HH.button
                  [ HP.classes [ CN.btn ]
                  , HE.onClick (HE.input_ (Raise Dismiss))
                  ]
                  [ HH.text (dismissMessage dlg) ]
              ]
          ]
      ]

  eval ∷ Query ~> H.ComponentDSL Unit Query Message Slam
  eval (Raise msg next) = do
    H.raise msg
    pure next

headerMessage ∷ Dialog → String
headerMessage (ConfirmDeletion _) = "Confirm group deletion"

dismissMessage ∷ Dialog → String
dismissMessage (ConfirmDeletion _) = "Cancel"

confirmMessage ∷ Dialog → String
confirmMessage (ConfirmDeletion _) = "Delete"

renderDialog ∷ Dialog → H.ComponentHTML Query
renderDialog = case _ of
  ConfirmDeletion group →
    HH.span_
      [ HH.text "Are you sure you want to delete the group "
      , HH.code_ [ HH.text (QAT.printGroupPath group) ]
      , HH.text "?"
      ]
