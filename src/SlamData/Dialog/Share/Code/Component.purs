module SlamData.Dialog.Share.Code.Component where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Canceler(), cancel)
import Control.Monad.Eff.Ref (Ref(), newRef, writeRef)
import Control.Monad.Eff.Exception (error)
import Control.UI.Browser as Br

import Data.Functor (($>))
import Data.Functor.Eff (liftEff)
import Data.Functor.Aff (liftAff)
import Data.Lens (LensP(), lens, (?~), (.~))
import Data.Maybe as M
import Data.Foldable as F

import DOM.HTML.Types (HTMLElement())

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as Cp
import Halogen.Component.Utils as Hu

import Quasar.Auth.Permission as Api

import SlamData.Effects (Slam(), SlamDataEffects())
import SlamData.Render.CSS as Rc

type State =
  {
    permissionToken :: M.Maybe Api.Permission
  , inputEl :: M.Maybe HTMLElement
  , valueRef :: M.Maybe (Ref (M.Maybe String))
  , canceler :: M.Maybe (Canceler SlamDataEffects)
  }

_permissionToken :: forall a r. LensP {permissionToken :: a|r} a
_permissionToken = lens _.permissionToken _{permissionToken = _}

_inputEl :: forall a r. LensP {inputEl :: a|r} a
_inputEl = lens _.inputEl _{inputEl = _}

_valueRef :: forall a r. LensP {valueRef :: a|r} a
_valueRef = lens _.valueRef _{valueRef = _}

_canceler :: forall a r. LensP {canceler :: a|r} a
_canceler = lens _.canceler _{canceler = _}

initialState :: State
initialState =
  {
    permissionToken: M.Nothing
  , inputEl: M.Nothing
  , valueRef: M.Nothing
  , canceler: M.Nothing
  }

data Query a
  = Generate a
  | Init HTMLElement a
  | GetPermissionToken (M.Maybe Api.Permission -> a)
  | SetCanceler (Canceler SlamDataEffects) a
  | Clear a

type ShareByCodeDSL = ComponentDSL State Query Slam

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state =
  H.form
    [ Cp.nonSubmit
    , P.classes [ Rc.tokenGeneratorForm ]
    ]
    [ H.div [ P.classes [ B.inputGroup ] ]
      [
        H.input [ P.classes [ B.formControl ]
                , P.value $ M.maybe "" Api.runPermission state.permissionToken
                , P.readonly true
                , P.placeholder "Generated token will appear here"
                , P.initializer (\el -> action $ Init el)
                , ARIA.label "Permission token"
                ]
      , H.img [ P.classes [ Rc.cancelInputRunIcon ]
              , P.src cancelIcon
              , E.onClick (E.input_ Clear)
              ]
      , H.span [ P.classes [ B.inputGroupBtn ] ]
        [ H.button
          [ P.classes [ B.btn, B.btnPrimary ]
          , E.onClick (E.input_ Generate)
          , P.disabled (M.isJust state.canceler)
          , ARIA.label "Generate token"
          ]
          [ H.text "Generate" ]
        ]
      ]
    ]
  where
  cancelIcon =
    if M.isJust state.canceler
      then "img/spin.gif"
      else "img/remove.svg"

eval :: Natural Query ShareByCodeDSL
eval (Generate next) = do
  gets _.canceler >>= \c -> when (M.isNothing c) do
    perm <- Hu.liftWithCanceler SetCanceler Api.genToken
    modify $ _permissionToken ?~ perm
    modify $ _canceler .~ M.Nothing
    gets _.inputEl >>= F.traverse_ (liftEff <<< Br.select)
    gets _.valueRef
      >>= F.traverse_ (liftEff <<< flip writeRef (pure $ Api.runPermission perm))
  pure next
eval (Init htmlEl next) = do
  modify $ _inputEl ?~ htmlEl
  valRef <- liftEff $ newRef M.Nothing
  modify $ _valueRef ?~ valRef
  pure next
eval (GetPermissionToken continue) =
  map continue $ gets _.permissionToken
eval (Clear next) = do
  gets _.canceler >>= F.traverse_ \c -> do
    liftAff $ cancel c $ error "Getting token has been canceled"
    modify $ _canceler .~ M.Nothing
  modify $ _permissionToken .~ M.Nothing
  pure next
eval (SetCanceler c next) =
  modify (_canceler ?~ c) $> next
