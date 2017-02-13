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
  ( component
  , Query(..)
  , AuthenticateOrPresentHelp(..)
  , State
  ) where

import SlamData.Prelude

import Control.UI.Browser as Browser
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff as Eff
import Control.Monad.Eff.Exception as Exception

import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Menu.Component as Menu
import Halogen.Query.EventSource as ES
import Halogen.HTML.Events as HE

import OIDC.Crypt as Crypt

import Quasar.Advanced.Types (ProviderR)

import SlamData.AuthenticationMode as AuthenticationMode
import SlamData.GlobalError (GlobalError)
import SlamData.GlobalError as GlobalError
import SlamData.GlobalMenu.Bus (SignInMessage(..))
import SlamData.Monad (Slam)
import SlamData.Quasar as Api
import SlamData.Quasar.Auth as Auth
import SlamData.Quasar.Auth.Authentication (AuthenticationError, toNotificationOptions)
import SlamData.Quasar.Auth.Store as AuthStore
import SlamData.Wiring as Wiring

data AuthenticateOrPresentHelp
  = Authenticate (Maybe ProviderR)
  | PresentHelp String

data Query a
  = DismissSubmenu a
  | HandleGlobalError GlobalError a
  | HandleMenuMessage (Menu.Message AuthenticateOrPresentHelp) a
  | Init a

type State =
  { loggedIn ∷ Boolean
  }


type HTML = H.ParentHTML Query (Menu.Query AuthenticateOrPresentHelp) Unit Slam
type DSL = H.ParentDSL State Query (Menu.Query AuthenticateOrPresentHelp) Unit Void Slam

component ∷ H.Component HH.HTML Query Unit Void Slam
component =
  H.lifecycleParentComponent
    { initialState: \_ → { loggedIn: false }
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes $ [ HH.ClassName "sd-global-menu" ] ]
    [ HH.slot unit Menu.component helpMenu $ HE.input HandleMenuMessage
    ]

eval ∷ Query ~> DSL
eval (Init next) = do
  { bus } ← H.lift Wiring.expose
  H.subscribe $ busEventSource (flip HandleGlobalError ES.Listening) bus.globalError
  update
  pure next
eval (DismissSubmenu next) = do
  H.query unit $ H.action $ Menu.DismissSubmenu
  pure next
eval (HandleGlobalError error next) = case error of
  GlobalError.Unauthorized _ → update $> next
  _ → pure next
eval (HandleMenuMessage (Menu.Selected a) next) = do
  case a of
    Authenticate providerR → authenticate providerR
    PresentHelp uri → presentHelp uri
  pure next

update ∷ DSL Unit
update = do
  maybeIdToken ← H.lift Auth.getIdToken
  case maybeIdToken of
    Just idToken → do
      either
        (const retrieveProvidersAndUpdateMenu)
        putEmailToMenu
        (Eff.runPure $ Exception.try $ Crypt.readPayload idToken)
    Nothing →
      retrieveProvidersAndUpdateMenu
  where
  putEmailToMenu ∷ Crypt.Payload → DSL Unit
  putEmailToMenu payload = do
    H.query unit
      $ H.action
      $ Menu.Set
        { chosen: Nothing
        , submenus:
            [ { label:
                fromMaybe "unknown user"
                $ map Crypt.runEmail
                $ Crypt.pluckEmail
                $ payload
              , submenu:
                [ { label: "🔒 Sign out"
                  , shortcutLabel: Nothing
                  , value: Authenticate Nothing
                  }
                ]
              }
            ]
            ⊕ helpMenu
        }
    H.modify _{ loggedIn = true }

  retrieveProvidersAndUpdateMenu ∷ DSL Unit
  retrieveProvidersAndUpdateMenu = void do
    eProviders ← Api.retrieveAuthProviders
    H.query unit
      $ H.action
      $ Menu.Set
          { chosen: Nothing
          , submenus: case eProviders of
              Right (Just providers) →
                let
                  makeSubmenuItem provider =
                    { label: "Sign in with " ⊕ provider.displayName
                    , shortcutLabel: Nothing
                    , value: Authenticate $ Just provider
                    }
                in
                  [ { label: "🔓 Sign in"
                    , submenu: makeSubmenuItem <$> providers
                    }
                  ]
                ⊕ helpMenu
              _ → helpMenu
          }

helpMenu ∷ Array (Menu.MenuItem AuthenticateOrPresentHelp)
helpMenu =
  [ { label: "Help"
    , submenu:
      [ { label: "User guide"
        , shortcutLabel: Nothing
        , value: PresentHelp "http://docs.slamdata.com/en/v3.0/users-guide.html"
        }
      , { label: "Administrator guide"
        , shortcutLabel: Nothing
        , value: PresentHelp "http://docs.slamdata.com/en/v3.0/administration-guide.html"
        }
      , { label: "Developer guide"
        , shortcutLabel: Nothing
        , value: PresentHelp "http://docs.slamdata.com/en/v3.0/developers-guide.html"
        }
      , { label: "Helpful tips"
        , shortcutLabel: Nothing
        , value: PresentHelp "http://docs.slamdata.com/en/v3.0/helpful-tips.html"
        }
      , { label: "SQL² reference"
        , shortcutLabel: Nothing
        , value: PresentHelp "http://docs.slamdata.com/en/v3.0/sql-squared-reference.html"
        }
      , { label: "SlamDown reference"
        , shortcutLabel: Nothing
        , value: PresentHelp "http://docs.slamdata.com/en/v3.0/slamdown-reference.html"
        }
      , { label: "Troubleshooting FAQ"
        , shortcutLabel: Nothing
        , value: PresentHelp "http://docs.slamdata.com/en/v3.0/troubleshooting-faq.html"
        }
      ]
    }
  ]

authenticate ∷ Maybe ProviderR → DSL Unit
authenticate = maybe logOut logIn
  where
  keySuffix ∷ String
  keySuffix =
    AuthenticationMode.toKeySuffix AuthenticationMode.ChosenProvider

  logOut ∷ DSL Unit
  logOut = H.liftEff do
    AuthStore.clearIdToken keySuffix
    AuthStore.clearUnhashedNonce keySuffix
    AuthStore.clearProvider keySuffix
    Browser.reload

  logIn ∷ ProviderR → DSL Unit
  logIn providerR = do
    { auth } ← H.lift Wiring.expose
    idToken ← H.liftAff AVar.makeVar
    H.liftAff $ Bus.write { providerR, idToken, prompt: true, keySuffix } auth.requestToken
    either signInFailure (const $ signInSuccess) =<< (H.liftAff $ AVar.takeVar idToken)

  -- TODO: Reattempt failed actions without loosing state, remove reload.
  signInSuccess ∷ DSL Unit
  signInSuccess = do
    { auth } ← H.lift Wiring.expose
    H.liftAff $ Bus.write SignInSuccess auth.signIn
    update
    H.liftEff Browser.reload

  signInFailure ∷ AuthenticationError → DSL Unit
  signInFailure error = do
    { auth, bus } ← H.lift Wiring.expose
    H.liftAff do
      maybe (pure unit) (flip Bus.write bus.notify) (toNotificationOptions error)
      Bus.write SignInFailure auth.signIn

presentHelp ∷ String → DSL Unit
presentHelp = H.liftEff ∘ Browser.newTab
