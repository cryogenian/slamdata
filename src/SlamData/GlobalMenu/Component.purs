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
  , MenuOpen(..)
  , Message(..)
  , AuthenticateOrPresentHelp(..)
  , State
  , module SlamData.GlobalMenu.Bus
  ) where

import SlamData.Prelude

import Control.UI.Browser as Browser
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff as Eff
import Control.Monad.Eff.Ref as Ref
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
import SlamData.Workspace.Eval.Card as EvalCard
import SlamData.GlobalError (GlobalError)
import SlamData.GlobalError as GlobalError
import SlamData.GlobalMenu.Bus (SignInMessage(..))
import SlamData.Monad (Slam)
import SlamData.Quasar as Api
import SlamData.Quasar.Auth as Auth
import SlamData.Quasar.Auth.Authentication (AuthenticationError, toNotificationOptions)
import SlamData.Quasar.Auth.Store as AuthStore
import SlamData.Render.Icon as I
import SlamData.Wiring as Wiring
import SlamData.Workspace.Eval.Persistence as Persistence

data AuthenticateOrPresentHelp
  = Authenticate (Maybe ProviderR)
  | PresentHelp String
  | PresentAttribution

data Query a
  = DismissSubmenu a
  | HandleGlobalError GlobalError a
  | HandleMenuMessage (Menu.Message AuthenticateOrPresentHelp) a
  | ToggleMenu MenuOpen a
  | Init a

data MenuOpen
 = SignInMenu
 | HelpMenu

derive instance eqMenuOpen ∷ Eq MenuOpen

type State =
  { loggedIn ∷ Boolean
  , menuOpen ∷ Maybe MenuOpen
  }

data Message =
  PresentAttributionsDialog

type HTML = H.ParentHTML Query (Menu.Query AuthenticateOrPresentHelp) Unit Slam
type DSL = H.ParentDSL State Query (Menu.Query AuthenticateOrPresentHelp) Unit Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.lifecycleParentComponent
    { initialState: \_ → { loggedIn: false, menuOpen: Nothing }
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
    $ helpMenu state.menuOpen

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    { bus } ← H.lift Wiring.expose
    H.subscribe $ busEventSource (flip HandleGlobalError ES.Listening) bus.globalError
    update
    pure next

  DismissSubmenu next → do
    _ ← H.query unit $ H.action $ Menu.DismissSubmenu
    pure next

  HandleGlobalError error next →
    case error of
      GlobalError.Unauthorized _ → update $> next
      _ → pure next

  HandleMenuMessage (Menu.Selected a) next → do
    case a of
      Authenticate providerR → authenticate providerR
      PresentHelp uri → presentHelp uri
      PresentAttribution → H.raise PresentAttributionsDialog
    pure next

  ToggleMenu which next → do
    menuOpen ← H.gets _.menuOpen
    let
      m = case menuOpen, which of
        Just HelpMenu, HelpMenu → Nothing
        Just SignInMenu, SignInMenu → Nothing
        _, _ → Just which
    H.modify _{ menuOpen = m }
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
    _ ← H.query unit
      $ H.action
      $ Menu.Set
        { chosen: Nothing
        , submenus:
            [ { label:
                fromMaybe "unknown user"
                $ map unwrap
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
        }
    H.modify _{ loggedIn = true }

  retrieveProvidersAndUpdateMenu ∷ DSL Unit
  retrieveProvidersAndUpdateMenu = void do
    eProviders ← Api.retrieveAuthProviders
    pure unit

    {- H.query unit
      $ H.action
      $ Menu.Set
          { chosen: Nothing
          , submenus: case eProviders of
              _ ->
              -- Right (Just providers) →
                let
                  makeSubmenuItem provider =
                    { label: "Sign in with " ⊕ provider.displayName
                    , shortcutLabel: Nothing
                    , value: Authenticate $ Just provider
                    }
                in
                  [ { label: "🔓 Sign in"
                    , submenu: makeSubmenuItem <$> mempty -- providers
                    }
                  ]
                ⊕
              -- _ → helpMenu
          }
                -}

helpMenu :: forall a. Maybe MenuOpen → Array (H.HTML a Query)
helpMenu whichMenu =
  let
    container = HH.ClassName "menu-container"

    -- wraps stuff in an li > a
    item attrs children =
      HH.li [ HP.classes $ HH.ClassName <$> [ "menu-item", "submenu-item" ] ]
        [ HH.a
          ([ HP.classes $ HH.ClassName <$> [ "menu-item-link", "submenu-item-link" ] ] <> attrs)
          children
        ]
  in
    [ HH.div
        [ HP.classes [ container, HH.ClassName "sign-in-menu-container" ] ]
        [ HH.button
            [ HP.class_ $ HH.ClassName "sign-in-menu-button "
            , HE.onClick $ HE.input_ $ ToggleMenu SignInMenu
            ]
            [ I.unlockSm, HH.text "Sign in" ]
        ]
    , HH.div
        [ HP.classes [ container, HH.ClassName "help-menu-container" ] ] $
        [ HH.button
          [ HP.class_ $ HH.ClassName "help-menu-button"
          , HE.onClick $ HE.input_ $ ToggleMenu HelpMenu
          ]
          [ I.helpSm, HH.text "Help" ]
        ] <>
          if whichMenu == Just HelpMenu then
            [ HH.ul
              [ HP.class_ $ HH.ClassName "submenu" ]
              [ item
                  [ HP.href "http://docs.slamdata.com/en/v4.2/users-guide.html" ]
                  [ HH.text "User guide" ]
              , item
                  [ HP.href "http://docs.slamdata.com/en/v4.2/administration-guide.html" ]
                  [ HH.text "Administrator guide" ]
              , item
                  [ HP.href "http://docs.slamdata.com/en/v4.2/developers-guide.html" ]
                  [ HH.text "Developer guide" ]
              , item
                  [ HP.href "http://docs.slamdata.com/en/v4.2/helpful-tips.html" ]
                  [ HH.text "Helpful tips" ]
              , item
                  [ HP.href "http://docs.slamdata.com/en/v4.2/sql-squared-reference.html" ]
                  [ HH.text "SQL² reference" ]
              , item
                  [ HP.href "http://docs.slamdata.com/en/v4.2/slamdown-reference.html" ]
                  [ HH.text "SlamDown reference" ]
              , item
                  [ HP.href "http://docs.slamdata.com/en/v4.2/troubleshooting-faq.html" ]
                  [ HH.text "Troubleshooting FAQ" ]
              ]
            ]
          else
            []
    ]

authenticate ∷ Maybe ProviderR → DSL Unit
authenticate = maybe logOut logIn
  where
  keySuffix ∷ String
  keySuffix =
    AuthenticationMode.toKeySuffix AuthenticationMode.ChosenProvider

  logOut ∷ DSL Unit
  logOut = do
    AuthStore.removeIdToken keySuffix
    AuthStore.removeUnhashedNonce keySuffix
    AuthStore.removeProvider keySuffix
    update

  logIn ∷ ProviderR → DSL Unit
  logIn providerR = do
    { auth } ← H.lift Wiring.expose
    idToken ← H.liftAff AVar.makeVar
    H.liftAff $ Bus.write { providerR, idToken, prompt: true, keySuffix } auth.requestToken
    either signInFailure (const $ signInSuccess) =<< (H.liftAff $ AVar.takeVar idToken)

  signInSuccess ∷ DSL Unit
  signInSuccess = do
    wiring ← Wiring.expose
    update
    traverse_ (lift ∘ Persistence.queueEvalImmediate ∘ EvalCard.toAll)
      =<< (H.liftEff $ Ref.readRef wiring.auth.retryEval)
    whenM
      (H.liftEff $ Ref.readRef wiring.auth.retrySave)
      (void $ lift $ Persistence.saveWorkspace)
    H.liftAff $ Bus.write SignInSuccess wiring.auth.signIn

  signInFailure ∷ AuthenticationError → DSL Unit
  signInFailure error = do
    { auth, bus } ← H.lift Wiring.expose
    H.liftAff do
      maybe (pure unit) (flip Bus.write bus.notify) (toNotificationOptions error)
      Bus.write SignInFailure auth.signIn

presentHelp ∷ String → DSL Unit
presentHelp = H.liftEff ∘ Browser.newTab
