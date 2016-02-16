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

import Prelude

import Data.Foldable (traverse_)
import Data.Functor.Eff (liftEff)
import Data.Maybe (Maybe(..))

import DOM.HTML.Types (HTMLElement())

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import SlamData.Config as Config
import SlamData.Effects (Slam())
import SlamData.Render.CSS as Rc

import Utils.DOM (focus, blur)

type State =
  { value :: String
  , el :: Maybe HTMLElement
  }

data Query a
  = Init HTMLElement a
  | SetText String a
  | Focus a
  | Submit a

type RenameDSL = ComponentDSL State Query Slam

initialState :: State
initialState =
  { value: ""
  , el: Nothing
  }

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state =
  H.div [ P.classes [ Rc.notebookName ] ]
  [ H.input [ P.value state.value
            , P.id_ Config.notebookNameEditorId
            , P.initializer \el -> action (Init el)
            , E.onValueInput (E.input SetText)
            , E.onValueChange (E.input_ Submit)
            ]
  ]

eval :: Natural Query RenameDSL
eval (Init el next) = do
  modify _{el = pure el}
  pure next
eval (SetText str next) = do
  modify _{value = str}
  pure next
eval (Submit next) = do
  gets _.el >>= traverse_ (liftEff <<< blur)
  pure next
eval (Focus next) = do
  gets _.el >>= traverse_ (liftEff <<< focus)
  pure next
