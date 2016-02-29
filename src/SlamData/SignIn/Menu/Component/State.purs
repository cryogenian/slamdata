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

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Component.State as HalogenMenuState
import Halogen.Menu.Submenu.Component.State as HalogenSubmenuState

import Quasar.Auth.Provider as Provider

type StateP g = HalogenMenu.MenuP (Maybe Provider.ProviderR) g

make
  :: Array Provider.Provider
  -> HalogenMenu.Menu (Maybe Provider.ProviderR)
make providers = HalogenMenuState.makeMenu
  [ { label: "🔓 Sign in"
    , submenu: makeSubmenuItem <$> providers
    }
  ]

makeSubmenuItem
  :: Provider.Provider
  -> HalogenSubmenuState.SubmenuItem (Maybe Provider.ProviderR)
makeSubmenuItem (Provider.Provider providerR) =
  { label: "Sign in with " ++ providerR.displayName
  , shortcutLabel: Nothing
  , value: Just providerR
  }
