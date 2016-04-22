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

module SlamData.Dialog.Share.Code.Component where

import SlamData.Prelude

import Control.Monad.Aff (Canceler, cancel)
import Control.Monad.Eff.Exception (error)
import Control.UI.Browser as Br

import Data.Lens (LensP, lens, (?~), (.~))

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.Component.Utils as HU
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam, SlamDataEffects)
import SlamData.Quasar.Auth.Permission as Api
import SlamData.Render.CSS as Rc

type State =
  {
    permissionToken :: Maybe Api.PermissionToken
  , inputEl :: Maybe HTMLElement
  , canceler :: Maybe (Canceler SlamDataEffects)
  , disabled :: Boolean
  }

_permissionToken :: forall a r. LensP {permissionToken :: a|r} a
_permissionToken = lens _.permissionToken _{permissionToken = _}

_inputEl :: forall a r. LensP {inputEl :: a|r} a
_inputEl = lens _.inputEl _{inputEl = _}

_canceler :: forall a r. LensP {canceler :: a|r} a
_canceler = lens _.canceler _{canceler = _}

_disabled :: forall a r. LensP {disabled :: a|r} a
_disabled = lens _.disabled _{disabled = _}

initialState :: State
initialState =
  {
    permissionToken: Nothing
  , inputEl: Nothing
  , canceler: Nothing
  , disabled: true
  }

data Query a
  = Generate a
  | Init (Maybe HTMLElement) a
  | GetPermissionToken (Maybe Api.PermissionToken -> a)
  | SetCanceler (Canceler SlamDataEffects) a
  | Clear a
  | Toggle Boolean a

type ShareByCodeDSL = H.ComponentDSL State Query Slam

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> H.ComponentHTML Query
render state =
  HH.form
    [ CP.nonSubmit
    , HP.classes [ Rc.tokenGeneratorForm ]
    ]
    [ HH.div
        [ HP.classes [ B.inputGroup ] ]
        [ HH.input
            [ HP.classes [ B.formControl ]
            , HP.value $ maybe "" Api.runPermissionToken state.permissionToken
            , HP.readonly true
            , HP.placeholder "Generated token will appear here"
            , HP.ref (H.action <<< Init)
            , ARIA.label "Permission token"
            ]
        , HH.img
            [ HP.classes [ Rc.cancelInputRunIcon ]
            , HP.src cancelIcon
            , HE.onClick (HE.input_ Clear)
            ]
        , HH.span
            [ HP.classes [ B.inputGroupBtn ] ]
            [ HH.button
                [ HP.classes [ B.btn, B.btnPrimary ]
                , HE.onClick (HE.input_ Generate)
                , HP.disabled (isJust state.canceler || state.disabled)
                , ARIA.label "Generate token"
                ]
                [ HH.text "Generate" ]
            ]
        ]
    ]
  where
  cancelIcon =
    if isJust state.canceler
      then "img/spin.gif"
      else "img/remove.svg"

eval :: Natural Query ShareByCodeDSL
eval (Generate next) = do
  H.gets _.canceler >>= \c -> when (isNothing c) do
    perm <- HU.liftWithCanceler SetCanceler Api.genToken
    H.modify $ _permissionToken ?~ perm
    H.modify $ _canceler .~ Nothing
    H.gets _.inputEl >>= traverse_ (H.fromEff <<< Br.select)
  pure next
eval (Init htmlEl next) = (H.modify $ _inputEl .~ htmlEl) $> next
eval (GetPermissionToken continue) =
  map continue $ H.gets _.permissionToken
eval (Clear next) = do
  H.gets _.canceler >>= traverse_ \c -> do
    H.fromAff $ cancel c $ error "Getting token has been canceled"
    H.modify $ _canceler .~ Nothing
  H.modify $ _permissionToken .~ Nothing
  pure next
eval (SetCanceler c next) = do
  H.gets _.canceler >>= traverse_ \c ->
    H.fromAff $ cancel c $ error "New token has been requested"
  H.modify (_canceler ?~ c) $> next
eval (Toggle toggled next) = H.modify (_disabled .~ not toggled) $> next
