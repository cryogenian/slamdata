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

module SlamData.Notebook.Rename.Component where

import SlamData.Prelude

import Data.String as Str
import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Config as Config
import SlamData.Effects (Slam)
import SlamData.Render.CSS as Rc

import Utils.DOM (focus, blur)

type State =
  { value :: String
  , el :: Maybe HTMLElement
  }

data Query a
  = Ref (Maybe HTMLElement) a
  | SetText String a
  | Focus a
  | Submit a

type RenameDSL = H.ComponentDSL State Query Slam

initialState :: State
initialState =
  { value: ""
  , el: Nothing
  }

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> H.ComponentHTML Query
render state =
  HH.div
    [ HP.classes [ Rc.notebookName ] ]
    [ HH.input
        [ HP.value state.value
        , HP.id_ Config.notebookNameEditorId
        , HP.ref (H.action <<< Ref)
        , HE.onValueInput (HE.input SetText)
        , HE.onValueChange (HE.input_ Submit)
        ]
    ]

eval :: Natural Query RenameDSL
eval (Ref el next) = H.modify (_{el = el}) $> next
eval (SetText str next) = do
  H.modify (_{value = Str.replace "/" "" str})
  pure next
eval (Submit next) = (H.gets _.el >>= traverse_ (H.fromEff <<< blur)) $> next
eval (Focus next) = (H.gets _.el >>= traverse_ (H.fromEff <<< focus)) $> next
