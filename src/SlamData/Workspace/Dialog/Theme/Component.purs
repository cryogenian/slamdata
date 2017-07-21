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

module SlamData.Workspace.Dialog.Theme.Component where

import SlamData.Prelude

import Halogen as H
--import Halogen.HTML.Events as HE
import Halogen.HTML as HH
--import Halogen.HTML.Properties as HP
--import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Monad (Slam)
--import SlamData.Render.ClassName as CN
import SlamData.Theme as Theme
--import Utils.DOM as DOM

type State =
  { theme ∷ Maybe Theme.Theme
  }

data Query a
  = UpdateTheme Theme.Theme a
  | Cancel a

data Message
  = Dismiss
  | Theme Theme.Theme

component ∷ H.Component HH.HTML Query (Maybe Theme.Theme) Message Slam
component =
  H.component
    { initialState: { theme: _ }
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ComponentHTML Query
render { theme } =
  HH.div_ [ HH.text "Hola mundo" ]

eval ∷ Query ~> H.ComponentDSL State Query Message Slam
eval = case _ of
  UpdateTheme theme next →
    H.modify _ { theme = pure theme } $> next
  Cancel next →
    H.raise Dismiss $> next
