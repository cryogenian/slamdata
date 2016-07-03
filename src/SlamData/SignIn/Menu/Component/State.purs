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

module SlamData.SignIn.Menu.Component.State where

import SlamData.Prelude

import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Component.State as HalogenMenuState
import Halogen.Menu.Submenu.Component.State as HalogenSubmenuState

import Quasar.Advanced.Types (ProviderR)

type StateP g = HalogenMenu.MenuP (Maybe ProviderR) g

make ∷ Array ProviderR → HalogenMenu.Menu (Maybe ProviderR)
make providers = HalogenMenuState.makeMenu
  [ { label: "🔓 Sign in"
    , submenu: makeSubmenuItem <$> providers
    }
  ]

makeSubmenuItem ∷ ProviderR → HalogenSubmenuState.SubmenuItem (Maybe ProviderR)
makeSubmenuItem provider =
  { label: "Sign in with " ⊕ provider.displayName
  , shortcutLabel: Nothing
  , value: Just provider
  }
