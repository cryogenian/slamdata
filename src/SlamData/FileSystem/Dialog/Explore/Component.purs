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

module SlamData.FileSystem.Dialog.Explore.Component where

import SlamData.Prelude

import Data.Path.Pathy as Pt

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Config as Config
import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.FileSystem.Dialog.Component.Message (Message(..))
import SlamData.Monad (Slam)
import SlamData.Render.Common (formGroup)

import Utils.Path as UP

type State =
  { filePath ∷ UP.FilePath
  , workspaceName ∷ String
  }

initialState ∷ UP.FilePath → State
initialState fp =
  { filePath: fp
  , workspaceName: Config.newWorkspaceName
  }

data Query a
  = TryExplore UP.FilePath String a
  | NameTyped String a
  | RaiseDismiss a

component ∷ H.Component HH.HTML Query UP.FilePath Message Slam
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ComponentHTML Query
render state =
  modalDialog
    [ modalHeader "Explore file"
    , modalBody
      $ HH.form_
          [ HH.h4_
              [ HH.text
                  $ "Create a new workspace in "
                  ⊕ directory
                  ⊕ " to explore "
                  ⊕ fileName
              ]
          , formGroup
              [ HH.input
                  [ HP.classes [ B.formControl ]
                  , HP.value state.workspaceName
                  , HP.placeholder "New workspace name"
                  , ARIA.label "New workspace name"
                  , HE.onValueInput (HE.input NameTyped)
                  ]
              ]
          ]
    , modalFooter
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ RaiseDismiss)
            , HP.type_ HP.ButtonButton
            ]
            [ HH.text "Cancel" ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ]
            , HP.disabled $ state.workspaceName == ""
            , HE.onClick (HE.input_ (TryExplore state.filePath state.workspaceName))
            , HP.type_ HP.ButtonButton
            , ARIA.label "Explore file"
            ]
            [ HH.text "Explore" ]
        ]
    ]
    where
    anyPath = Right state.filePath
    directory = Pt.printPath $ UP.getDir anyPath
    fileName = UP.getNameStr anyPath

eval ∷ Query ~> H.ComponentDSL State Query Message Slam
eval = case _ of
  RaiseDismiss next → do
    H.raise Dismiss
    pure next
  TryExplore res name next → do
    H.raise $ ExploreFile res name
    pure next
  NameTyped name next → do
    H.modify (_ { workspaceName = name })
    pure next
