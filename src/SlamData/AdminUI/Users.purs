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

module SlamData.AdminUI.Users where

import SlamData.Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.AdminUI.Types as AT
import SlamData.Render.Common as R

renderForm ∷ AT.UsersState → Array AT.HTML
renderForm (AT.UsersState state) =
  [ HH.fieldset_
      [ HH.label_
          [ HH.text "Search"
          , HH.input
              [ HP.class_ (HH.ClassName "form-control")
              , HP.type_ HP.InputText
              , HP.placeholder "Search string"
              , HE.onValueInput $ HE.input \str → AT.SetUsers (AT.UsersState (state {search = str}))
              , HP.value state.search
              ]
          , HH.button
              [ HE.onClick $ HE.input_ (AT.SetUsers (AT.UsersState (state {search = ""})))
              ]
              [ R.clearFieldIcon "Clear search string" ]
          ]
      ]
  ]
