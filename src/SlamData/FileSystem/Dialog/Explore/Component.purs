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

import Data.Lens (LensP, lens, (.~))
import Data.Path.Pathy as Pt

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Monad (Slam)
import SlamData.Render.Common (formGroup)
import SlamData.Config as Config

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


_filePath ∷ ∀ a r. LensP {filePath ∷ a |r} a
_filePath = lens (_.filePath) (_{filePath = _})

_workspaceName ∷ ∀ a r. LensP {workspaceName ∷ a|r} a
_workspaceName = lens (_.workspaceName) (_{workspaceName = _})


data Query a
  = Explore UP.FilePath String a
  | NameTyped String a
  | Dismiss a

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → H.ComponentHTML Query
render state =
  modalDialog
    [ modalHeader "Explore file"
    , modalBody
      $ HH.form_
          [ HH.h4_ [ HH.text
                       $ "Create a new workspace in "
                       ⊕ directory
                       ⊕ " to explore "
                       ⊕ fileName ]
          , formGroup
              [ HH.input [ HP.classes [ B.formControl ]
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
            , HE.onClick (HE.input_ Dismiss)
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Cancel" ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ]
            , HP.disabled $ state.workspaceName == ""
            , HE.onClick (HE.input_ (Explore state.filePath state.workspaceName))
            , HP.buttonType HP.ButtonButton
            , ARIA.label "Explore file"
            ]
            [ HH.text "Explore" ]
        ]
    ]
    where
    anyPath = Right state.filePath
    directory = Pt.printPath $ UP.getDir anyPath
    fileName = UP.getNameStr anyPath

eval ∷ Query ~> (H.ComponentDSL State Query Slam)
eval (Dismiss next) = pure next
eval (Explore res name next) = pure next
eval (NameTyped name next) = H.modify (_workspaceName .~ name) $> next
