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

module SlamData.GlobalMenu.Component
  ( comp
  , MenuSlot
  , QueryP
  , StateP
  , ChildQuery
  , Query(..)
  , ChildSlot
  , ChildState
  , module SlamData.GlobalMenu.Component.State
  ) where

import SlamData.Prelude

import Data.Time.Duration (Milliseconds(Milliseconds))

import Control.UI.Browser as Browser
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus as Bus

import Halogen as H
import Halogen.HTML.Core (className)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Menu.Component (MenuQuery(..), menuComponent) as HalogenMenu
import Halogen.Menu.Component.State (MenuItem, makeMenu)
import Halogen.Menu.Submenu.Component (SubmenuQuery(..)) as HalogenMenu

import OIDC.Crypt as Crypt

import Quasar.Advanced.Types (ProviderR)

import SlamData.Monad (Slam)
import SlamData.Quasar as Api
import SlamData.Notification (NotificationOptions)
import SlamData.Notification as Notification
import SlamData.Quasar.Auth as Auth
import SlamData.Quasar.Auth.Authentication (AuthenticationError(..))
import SlamData.Quasar.Auth.Store as AuthStore
import SlamData.GlobalMenu.Bus (SignInMessage(..))
import SlamData.GlobalMenu.Component.State (State, initialState)
import SlamData.GlobalMenu.Menu.Component.Query (QueryP) as MenuQuery
import SlamData.GlobalMenu.Menu.Component.State as MenuState
import SlamData.Wiring (WiringR)
import SlamData.Wiring as Wiring


data Query a
  = DismissSubmenu a
  | Init a

type QueryP = Coproduct Query (H.ChildF MenuSlot ChildQuery)

data MenuSlot = MenuSlot

derive instance genericMenuSlot ∷ Generic MenuSlot
derive instance eqMenuSlot ∷ Eq MenuSlot
derive instance ordMenuSlot ∷ Ord MenuSlot

type ChildSlot = MenuSlot

type ChildQuery = MenuQuery.QueryP

type ChildState g = MenuState.StateP g

type StateP = H.ParentState State (ChildState Slam) Query ChildQuery Slam ChildSlot
type GlobalMenuHTML = H.ParentHTML (ChildState Slam) Query ChildQuery Slam ChildSlot
type GlobalMenuDSL = H.ParentDSL State (ChildState Slam) Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.lifecycleParentComponent
    { render
    , eval
    , peek: Just (menuPeek ∘ H.runChildF)
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render ∷ State → GlobalMenuHTML
render state =
  HH.div
    [ HP.classes $ [ className "sd-global-menu" ] ]
    [ HH.slot MenuSlot \_ →
        { component: HalogenMenu.menuComponent
        , initialState: H.parentState $ makeMenu helpMenu
        }
    ]

eval ∷ Query ~> GlobalMenuDSL
eval (DismissSubmenu next) = dismissAll $> next
eval (Init next) = update $> next

update ∷ GlobalMenuDSL Unit
update = do
  mbIdToken ← H.liftH $ H.liftH $ Auth.getIdToken
  maybe
    retrieveProvidersAndUpdateMenu
    putEmailToMenu
    mbIdToken
  where
  putEmailToMenu ∷ Crypt.IdToken → GlobalMenuDSL Unit
  putEmailToMenu token = do
    queryMenu
      $ H.action
      $ HalogenMenu.SetMenu
      $ makeMenu
      $ [ { label:
              fromMaybe "unknown user"
              $ map Crypt.runEmail
              $ Crypt.pluckEmail token
          , submenu:
              [ { label: "🔒 Sign out"
                , shortcutLabel: Nothing
                , value: MenuState.Authenticate Nothing
                }
              ]
          }
        ] <> helpMenu
    H.modify (_{loggedIn = true})

  retrieveProvidersAndUpdateMenu ∷ GlobalMenuDSL Unit
  retrieveProvidersAndUpdateMenu = do
    eProviders ← Api.retrieveAuthProviders
    queryMenu
      $ H.action
      $ HalogenMenu.SetMenu
      $ makeMenu
      $ case eProviders of
          Right (Just providers) →
            [ { label: "🔓 Sign in"
              , submenu: MenuState.makeAuthenticateSubmenuItem <$> providers
              }
            ] <> helpMenu
          _ → helpMenu

helpMenu ∷ Array (MenuItem MenuState.AuthenticateOrPresentHelp)
helpMenu =
  [ { label: "Help"
    , submenu:
      [ { label: "User guide"
        , shortcutLabel: Nothing
        , value:
            MenuState.PresentHelp
              "http://docs.slamdata.com/en/v3.0/users-guide.html"
        }
      , { label: "Administrator guide"
        , shortcutLabel: Nothing
        , value:
            MenuState.PresentHelp
              "http://docs.slamdata.com/en/v3.0/administration-guide.html"
        }
      , { label: "Developer guide"
        , shortcutLabel: Nothing
        , value:
            MenuState.PresentHelp
              "http://docs.slamdata.com/en/v3.0/developers-guide.html"
        }
      , { label: "Helpful tips"
        , shortcutLabel: Nothing
        , value:
            MenuState.PresentHelp
              "http://docs.slamdata.com/en/v3.0/helpful-tips.html"
        }
      , { label: "SQL² reference"
        , shortcutLabel: Nothing
        , value:
            MenuState.PresentHelp
              "http://docs.slamdata.com/en/v3.0/sql-squared-reference.html"
        }
      , { label: "SlamDown reference"
        , shortcutLabel: Nothing
        , value:
            MenuState.PresentHelp
              "http://docs.slamdata.com/en/v3.0/slamdown-reference.html"
        }
      , { label: "Troubleshooting FAQ"
        , shortcutLabel: Nothing
        , value:
            MenuState.PresentHelp
              "http://docs.slamdata.com/en/v3.0/troubleshooting-faq.html"
        }
      ]
    }
  ]

dismissAll ∷ GlobalMenuDSL Unit
dismissAll =
  queryMenu $
    H.action HalogenMenu.DismissSubmenu

menuPeek ∷ ∀ a. MenuQuery.QueryP a → GlobalMenuDSL Unit
menuPeek =
  coproduct
    (const (pure unit))
    (submenuPeek ∘ H.runChildF)

submenuPeek
  ∷ ∀ a
  . HalogenMenu.SubmenuQuery MenuState.AuthenticateOrPresentHelp a
  → GlobalMenuDSL Unit
submenuPeek (HalogenMenu.SelectSubmenuItem authenticateOrPresentHelp _) = do
   case authenticateOrPresentHelp of
     MenuState.Authenticate providerR → authenticate providerR
     MenuState.PresentHelp uri → presentHelp uri

queryMenu
  ∷ HalogenMenu.MenuQuery MenuState.AuthenticateOrPresentHelp Unit
  → GlobalMenuDSL Unit
queryMenu q = void $ H.query MenuSlot (left q)

authenticate ∷ Maybe ProviderR → GlobalMenuDSL Unit
authenticate =
  maybe logOut logIn
  where
  logOut ∷ GlobalMenuDSL Unit
  logOut = do
    H.fromEff do
      AuthStore.clearIdToken
      AuthStore.clearUnhashedNonce
      AuthStore.clearProvider
      Browser.reload

  logIn ∷ ProviderR → GlobalMenuDSL Unit
  logIn providerR = do
    wiringR ← H.liftH $ H.liftH $ Wiring.run <$> ask
    idToken ← H.fromAff AVar.makeVar
    H.fromAff $ Bus.write { providerR, idToken, prompt: true } wiringR.requestNewIdTokenBus
    either (signInFailure wiringR) (const $ signInSuccess wiringR)
      =<< (H.fromAff $ AVar.takeVar idToken)

  -- TODO: Reattempt failed actions without loosing state, remove reload.
  signInSuccess ∷ WiringR → GlobalMenuDSL Unit
  signInSuccess wiringR =
    (H.fromAff $ Bus.write SignInSuccess $ wiringR.signInBus)
      *> update
      *> H.fromEff Browser.reload

  signInFailure ∷ WiringR → AuthenticationError → GlobalMenuDSL Unit
  signInFailure wiringR error = do
    H.fromAff $ (Bus.write (toNotificationOptions error) wiringR.notify)
    H.fromAff $ (Bus.write SignInFailure $ wiringR.signInBus)

  toNotificationOptions ∷ AuthenticationError → NotificationOptions
  toNotificationOptions =
    case _ of
      IdTokenInvalid →
        { notification: Notification.Error $ "Sign in failed: Authentication provider provided invalid id token."
        , detail: Nothing
        , timeout
        }
      IdTokenUnavailable detail →
        { notification: Notification.Error $ "Sign in failed: Authentication provider didn't provide a token."
        , detail: Just detail
        , timeout
        }
      PromptDismissed →
        { notification: Notification.Warning $ "Sign in prompt closed."
        , detail: Nothing
        , timeout
        }
      DOMError detail →
        { notification: Notification.Error $ "Sign in failed: Your browser is incompatible with SlamData please try again with another browser."
        , detail: Just detail
        , timeout
        }
      ProviderError detail →
        { notification: Notification.Error $ "Sign in failed: There was a problem with your provider configuration, please update your SlamData configuration and try again."
        , detail: Just detail
        , timeout
        }
    where
    timeout = Just $ Milliseconds 10000.0

presentHelp ∷ String → GlobalMenuDSL Unit
presentHelp = H.fromEff ∘ Browser.newTab
