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

module SlamData.SignIn.Component
  ( comp
  , MenuSlot()
  , QueryP()
  , StateP()
  , ChildQuery()
  , Query(..)
  , ChildSlot()
  , ChildState()
  , module SlamData.SignIn.Component.State
  ) where

import Prelude

import Control.UI.Browser (reload)
import Control.Monad.Aff (attempt)
import Control.MonadPlus (guard)

import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Data.Functor.Eff (liftEff)
import Data.Functor.Aff (liftAff)
import Data.Either as E
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe as M

import Halogen
import Halogen.HTML.Core (className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Component.State as HalogenMenu
import Halogen.Menu.Submenu.Component as HalogenMenu

import OIDC.Aff (requestAuthentication)
import OIDCCryptUtils as Crypt
import SlamData.Effects (Slam())
import SlamData.SignIn.Component.State
import SlamData.SignIn.Menu.Component.Query as Menu
import SlamData.SignIn.Menu.Component.State as Menu
import Quasar.Auth.Provider as Provider
import Quasar.Auth as Auth
import Quasar.Aff as Api

data Query a
  = DismissSubmenu a
  | Init a

type QueryP = Coproduct Query (ChildF MenuSlot ChildQuery)

data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type ChildSlot = MenuSlot

type ChildQuery = Menu.QueryP

type ChildState g = Menu.StateP g

type StateP = InstalledState State (ChildState Slam) Query ChildQuery Slam ChildSlot
type SignInHTML = ParentHTML (ChildState Slam) Query ChildQuery Slam ChildSlot
type SignInDSL = ParentDSL State (ChildState Slam) Query ChildQuery Slam ChildSlot

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: State -> SignInHTML
render state =
  H.div
    [
      P.classes $ [ className "sd-sign-in" ]
    , P.initializer (\_ -> action Init)
    ]
    $ guard (not state.hidden)
    $> H.slot MenuSlot \_ ->
        { component: HalogenMenu.menuComponent
        , initialState: installedState $ Menu.make []
        }

eval :: Natural Query SignInDSL
eval (DismissSubmenu next) = dismissAll $> next
eval (Init next) = do
  mbIdToken <- liftEff Auth.retrieveIdToken
  M.maybe
    retrieveProvidersAndUpdateMenu
    putEmailToMenu
    mbIdToken
  pure next
  where
  putEmailToMenu :: Crypt.IdToken -> SignInDSL Unit
  putEmailToMenu token = do
    query MenuSlot
      $ left
      $ action
      $ HalogenMenu.SetMenu
      $ HalogenMenu.makeMenu
        [ { label:
              M.fromMaybe "unknown user"
              $ map Crypt.runEmail
              $ Crypt.pluckEmail token
          , submenu:
              [ { label: "🔒 Sign out"
                , shortcutLabel: M.Nothing
                , value: M.Nothing
                }
              ]
          }
        ]
    modify $ \x -> x{loggedIn = true}

  retrieveProvidersAndUpdateMenu :: SignInDSL Unit
  retrieveProvidersAndUpdateMenu = do
    eProviders <- liftAff $ attempt $ Api.retrieveAuthProviders
    case eProviders of
      E.Left _ -> modify \x -> x{hidden = true}
      E.Right M.Nothing -> modify \x -> x{hidden = true}
      E.Right (M.Just []) -> modify \x -> x{hidden = true}
      E.Right (M.Just providers) ->
        void
        $ query MenuSlot
        $ left
        $ action
        $ HalogenMenu.SetMenu
        $ HalogenMenu.makeMenu
          [ { label: "🔓 Sign in"
            , submenu: Menu.makeSubmenuItem <$> providers
            }
          ]

dismissAll :: SignInDSL Unit
dismissAll =
  queryMenu $
    action HalogenMenu.DismissSubmenu

makeAuthRequestWithProviderR
  :: M.Maybe Provider.ProviderR -> SignInDSL Unit
makeAuthRequestWithProviderR (M.Just pr) =
  liftEff $ requestAuthentication pr
makeAuthRequestWithProviderR _ =
  pure unit

peek
  :: forall a. ChildF ChildSlot ChildQuery a
  -> SignInDSL Unit
peek (ChildF p q) =
  menuPeek q

menuPeek
  :: forall a. Menu.QueryP a
  -> SignInDSL Unit
menuPeek =
  coproduct
    (const (pure unit))
    submenuPeek

evaluateMenuValue
  :: (M.Maybe Provider.ProviderR)
  -> SignInDSL Unit
evaluateMenuValue _ =
  pure unit

submenuPeek
  :: forall a
   . ChildF HalogenMenu.SubmenuSlotAddress
      (HalogenMenu.SubmenuQuery (M.Maybe Provider.ProviderR)) a
  -> SignInDSL Unit
submenuPeek (ChildF _ (HalogenMenu.SelectSubmenuItem v _)) = do
  {loggedIn} <- get
  if loggedIn
    then logOut
    else makeAuthRequestWithProviderR v
  where
  logOut :: SignInDSL Unit
  logOut = do
    liftEff do
      Auth.clearIdToken
      reload


queryMenu
  :: HalogenMenu.MenuQuery (M.Maybe Provider.ProviderR) Unit
  -> SignInDSL Unit
queryMenu q = void $ query MenuSlot (left q)
