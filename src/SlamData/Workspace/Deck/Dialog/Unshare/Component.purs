module SlamData.Workspace.Deck.Dialog.Unshare.Component where

import SlamData.Prelude

import Control.Monad.Aff (later') -- for mock
import Control.UI.Browser (select)
import Control.UI.ZClipboard as Z

import Data.Array as Arr
import Data.Foldable as F
import Data.Lens (LensP, lens, (.~), (%~), (?~))
import Data.Lens.Index (ix)

import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as Cp

import SlamData.Effects (Slam)
import SlamData.Render.Common (glyph)

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

data ShareResume
  = View
  | Edit

derive instance shareResumeEq ∷ Eq ShareResume

printShareResume ∷ ShareResume → String
printShareResume View = "View"
printShareResume Edit = "Edit"

type PermissionId = String
type TokenId = String

data PermissionState
  = Unsharing
  | Modifying
  | RevokeError
  | ModifyError

derive instance permissionStateEq ∷ Eq PermissionState

type Permission =
  { name ∷ String
  , resume ∷ ShareResume
  , permissionId ∷ PermissionId
  , state ∷ Maybe PermissionState
  }

type TokenPermission =
  { name ∷ Maybe String
  , secret ∷ String
  , resume ∷ ShareResume
  , tokenId ∷ String
  , state ∷ Maybe PermissionState
  }

_resume ∷ ∀ a r. LensP {resume ∷ a|r} a
_resume = lens (_.resume) (_{resume = _})

_name ∷ ∀ a r. LensP {name ∷ a |r} a
_name = lens (_.name) (_{name = _})

_permissionId ∷ ∀ a r. LensP {permissionId ∷ a |r} a
_permissionId = lens (_.permissionId) (_{permissionId = _})

_tokenId ∷ ∀ a r. LensP {tokenId ∷ a|r} a
_tokenId = lens (_.tokenId) (_{tokenId = _})

_secret ∷ ∀ a r. LensP {secret ∷ a|r} a
_secret = lens (_.secret) (_{secret = _})


-- :( `ix` doesn't work without `Maybe PermissionState`
_state ∷ ∀ r. LensP {state ∷ (Maybe PermissionState)|r} (Maybe PermissionState)
_state = lens (_.state) (_{state = _})

type State =
  { userPermissions ∷ Array Permission
  , tokenPermissions ∷ Array TokenPermission
  , groupPermissions ∷ Array Permission
  , loading ∷ Boolean
  }

initialState ∷ State
initialState =
  { userPermissions: [ ]
  , tokenPermissions: [ ]
  , groupPermissions: [ ]
  , loading: true
  }

_userPermissions ∷ ∀ a r. LensP {userPermissions ∷ a|r} a
_userPermissions = lens (_.userPermissions) (_{userPermissions = _})

_groupPermissions ∷ ∀ a r. LensP {groupPermissions ∷ a |r} a
_groupPermissions = lens (_.groupPermissions) (_{groupPermissions = _})

_tokenPermissions ∷ ∀ a r. LensP {tokenPermissions ∷ a|r} a
_tokenPermissions = lens (_.tokenPermissions) (_{tokenPermissions = _})

_loading ∷ ∀ a r. LensP {loading ∷ a|r} a
_loading = lens (_.loading) (_{loading = _})

data Query a
  = Dismiss a
  | InitZClipboard String (Maybe HTMLElement) a
  | Init a
  | SelectElement HTMLElement a
  | PermissionResumeChanged PermissionId String a
  | Unshare PermissionId a
  | UnshareToken String  a


comp ∷ H.Component State Query Slam
comp =
  H.lifecycleComponent
    { render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render ∷ State → HTML
render state =
  HH.div [ HP.classes [ HH.className "deck-dialog-unshare" ] ]
    [ HH.h4_ [ HH.text "Unshare deck" ]
    , HH.div
        [ HP.classes
            $ [ B.alert, B.alertInfo, HH.className "share-loading" ]
            ⊕ if state.loading then [ ] else [ B.hidden ]
        ]
        [ HH.img [ HP.src "img/blue-spin.svg" ]
        , HH.text "Loading"
        ]

    , HH.div
        [ HP.classes
            $ [ HH.className "deck-dialog-body" ]
            ⊕ (if state.loading then [ B.hidden ] else [ ])
        ]
        [ HH.form
            [ Cp.nonSubmit ]
            $ (if Arr.null state.userPermissions
                 then [ ]
                 else
                 [ HH.label
                     [ HP.classes [ HH.className "subject-label" ] ]
                     [ HH.text "Users" ]
                 ]
                 ⊕ map renderUserOrGroup state.userPermissions
              )
            ⊕ (if Arr.null state.groupPermissions
                 then [ ]
                 else
                 [ HH.label
                    [ HP.classes [ HH.className "subject-label" ] ]
                    [ HH.text "Groups" ]
                 ]
                 ⊕ map renderUserOrGroup state.groupPermissions
              )
            ⊕ (if Arr.null state.tokenPermissions
                 then [ ]
                 else
                   [ HH.label
                       [ HP.classes [ HH.className "subject-label" ] ]
                       [ HH.text "Tokens" ]
                   ]
                   ⊕ map renderToken state.tokenPermissions
              )
            ⊕ (if Arr.null state.userPermissions
                  ∧ Arr.null state.groupPermissions
                  ∧ Arr.null state.tokenPermissions
                 then
                   [ HH.p_ [ HH.text "This deck has no shared permissions" ] ]
                 else [ ]
              )
        ]
    , let
        states =
          (_.state <$> state.userPermissions)
          ⊕ (_.state <$> state.groupPermissions)
          ⊕ (_.state <$> state.tokenPermissions)
        somethingErrored =
          F.any (\x → x ≡ Just ModifyError ∨ x ≡ Just RevokeError) states
        somethingHappening =
          F.any (\x → x ≡ Just Unsharing ∨ x ≡ Just Modifying) states
      in
       HH.div
        [ HP.classes
            $ [ HH.className "deck-dialog-footer" ]
            ⊕ (if somethingErrored then [ B.hasError ] else [ ])
            ⊕ (if state.loading then [ B.hidden ] else [ ])
        ]
        [ HH.div
            [ HP.classes
                $ [ B.alert, B.alertDanger ]
                ⊕ (if somethingErrored then [ ] else [ B.hidden ])
            ]
            [ HH.text
                $ "This action couldn't be performed. "
                ⊕ "Please check your network connection and try again"
            ]
          , HH.button
            [ HE.onClick (HE.input_ Dismiss)
            , HP.buttonType HP.ButtonButton
            , HP.classes [ B.btn, B.btnDefault ]
            , HP.disabled somethingHappening
            ]
            [ HH.text "Done" ]
        ]
    ]

renderUserOrGroup ∷ Permission → HTML
renderUserOrGroup perm =
  HH.div
    [ HP.classes [ B.row ] ]
    [ HH.div
        [ HP.classes [ B.colXs7 ] ]
        [ HH.text perm.name ]
    , HH.div
        [ HP.classes
            $ [ B.colXs3 ]
            ⊕ if perm.state ≡ Just ModifyError then [ B.hasError ] else [ ]
        ]
        [ HH.select
            [ HP.classes [ B.formControl ]
            , HE.onValueChange (HE.input (PermissionResumeChanged perm.permissionId))
            , HP.disabled $ perm.state ≡ Just Modifying ∨ perm.state ≡ Just Unsharing
            ]
            [ HH.option
                [ HP.value "view"
                , HP.selected $ perm.resume ≡ View
                ]
                [ HH.text
                    $ "View"
                    ⊕ if perm.state ≡ Just Modifying
                        then "..."
                        else ""
                ]
            , HH.option
                [ HP.value "edit"
                , HP.selected $ perm.resume ≡ Edit
                ]
                [ HH.text
                    $ "Edit"
                    ⊕ if perm.state ≡ Just Modifying
                        then "..."
                        else ""
                ]
            ]
        ]
    , HH.div
        [ HP.classes
            $ [ B.colXs2 ]
            ⊕ if perm.state ≡ Just RevokeError then [ B.hasError ] else [ ]
        ]
        [ HH.button
            [ HP.classes [ B.btn, B.btnDefault, HH.className "unshare-button" ]
            , HE.onClick (HE.input_ (Unshare perm.permissionId))
            , HP.disabled $ perm.state ≡ Just Modifying ∨ perm.state ≡ Just Unsharing
            ]
            [ HH.text if perm.state ≡ Just Unsharing
                        then "Revoking..."
                        else "Unshare"
            ]
        ]
    ]

renderToken ∷ TokenPermission → HTML
renderToken token =
  HH.div
    [ HP.classes [ B.row ] ]
    [ HH.div
        [ HP.classes [ B.colXs4 ] ]
        [ HH.text $ fromMaybe "Untitled token" token.name ]
    , HH.div
        [ HP.classes [ B.colXs1 ] ]
        [ HH.text $ printShareResume token.resume ]
    , HH.div
        [ HP.classes [ B.colXs5 ] ]
        [ HH.div
            [ HP.classes
                $ [ B.inputGroup ]
                ⊕ if token.state ≡ Just ModifyError then [ B.hasError ] else [ ]
            ]
            [ HH.input
                [ HP.classes [ B.formControl ]
                , HP.readonly true
                , HP.title "Token secret"
                , HP.value token.secret
                , HE.onClick $ HE.input (SelectElement ∘ _.target)
                , HP.disabled $ isJust token.state
            ]
            , HH.span
                [ HP.classes [ B.inputGroupBtn ] ]
                [ HH.button
                    [ HP.classes [ B.btn, B.btnDefault ]
                    , HP.ref (H.action ∘ InitZClipboard token.secret)
                    , HP.disabled $ token.state ≡ Just Modifying ∨ token.state ≡ Just Unsharing
                    ]
                    [ glyph B.glyphiconCopy ]
                ]
            ]
        ]
    , HH.div
        [ HP.classes
            $ [ B.colXs2 ]
            ⊕ if token.state ≡ Just RevokeError then [ B.hasError ] else [ ]
        ]
        [ HH.button
            [ HP.classes [ B.btn, B.btnDefault, HH.className "unshare-button" ]
            , HE.onClick (HE.input_ (UnshareToken token.tokenId))
            , HP.disabled $ token.state ≡ Just Modifying ∨ token.state ≡ Just Unsharing
            ]
            [ HH.text if token.state ≡ Just Unsharing
                        then "Revoking..."
                        else "Unshare"
            ]
        ]
    ]


eval ∷ Query ~> DSL
eval (Init next) = next <$ do
  allTokens ← getAllTokens
  permissions ← getAllPermissions
  let
    adjustedPermissions = adjustPermissions allTokens permissions
  H.fromAff $ later' 3000 $ slamUnit
  H.modify (_{ userPermissions = adjustedPermissions.users
             , tokenPermissions = adjustedPermissions.tokens
             , groupPermissions = adjustedPermissions.groups
             , loading = false
             })

  pure unit
eval (InitZClipboard token mbEl next) =
  next <$ for_ mbEl \el → do
    H.fromEff $ Z.make (htmlElementToElement el)
      >>= Z.onCopy (Z.setData "text/plain" token)
eval (Dismiss next) = pure next
eval (SelectElement el next) =
  next <$ H.fromEff (select el)
eval (PermissionResumeChanged permId string next) = next <$ do
  state ← H.get
  let
    muix = Arr.findIndex (\x → x.permissionId ≡ permId) state.userPermissions
    mgix = Arr.findIndex (\x → x.permissionId ≡ permId) state.groupPermissions
    newResume | string ≡ "view" = View
              | otherwise = Edit

  for_ muix \uix →
    H.modify
      $ _userPermissions ∘ ix uix ∘ _state ?~ Modifying

  for_ mgix \gix →
    H.modify
      $ _groupPermissions ∘ ix gix ∘ _state ?~ Modifying


  changePermissionResume newResume permId

  for_ muix \uix →
    H.modify
      $ (_userPermissions ∘ ix uix ∘ _resume .~ newResume)
      ∘ (_userPermissions ∘ ix uix ∘ _state .~ Nothing)

  for_ mgix \gix →
    H.modify
      $ (_groupPermissions ∘ ix gix ∘ _resume .~ newResume)
      ∘ (_groupPermissions ∘ ix gix ∘ _state .~ Nothing)

eval (UnshareToken tokenId next) = next <$ do
  state ← H.get
  let
    mtix = Arr.findIndex (\x → x.tokenId ≡ tokenId) state.tokenPermissions

  for_ mtix \tix →
    H.modify $ _tokenPermissions ∘ ix tix ∘ _state ?~ Unsharing

  deleteToken tokenId

  H.modify
    $ _tokenPermissions %~ Arr.filter (\x → x.tokenId ≠ tokenId)

  for_ mtix \tix →
    H.modify $ _tokenPermissions ∘ ix tix ∘ _state .~ Nothing

eval (Unshare permId next) = next <$ do
  state ← H.get
  let
    muix = Arr.findIndex (\x → x.permissionId ≡ permId) state.userPermissions
    mgix = Arr.findIndex (\x → x.permissionId ≡ permId) state.groupPermissions

  for_ muix \uix →
    H.modify $ _userPermissions ∘ ix uix ∘ _state ?~ Unsharing
  for_ mgix \gix →
    H.modify $ _groupPermissions ∘ ix gix ∘ _state ?~ Unsharing

  deletePermission permId

  H.modify $ _userPermissions %~ Arr.filter (\x → x.permissionId ≠ permId)
  H.modify $ _groupPermissions %~ Arr.filter (\x → x.permissionId ≠ permId)


  for_ muix \uix →
    H.modify $ _userPermissions ∘ ix uix ∘ _state .~ Nothing
  for_ mgix \gix →
    H.modify $ _groupPermissions ∘ ix gix ∘ _state .~ Nothing

getAllTokens ∷ DSL Unit
getAllTokens =
  Debug.Trace.traceAnyA "get all tokens"

getAllPermissions ∷ DSL Unit
getAllPermissions =
  Debug.Trace.traceAnyA "get all permissions"

slamUnit ∷ Slam Unit
slamUnit = pure unit

changePermissionResume ∷ ShareResume → PermissionId → DSL Unit
changePermissionResume res pid = do
  Debug.Trace.traceAnyA "change resume"
  Debug.Trace.traceAnyA res
  Debug.Trace.traceAnyA pid
  H.fromAff $ later' 2000 slamUnit


deleteToken ∷ String → DSL Unit
deleteToken tid = do
  Debug.Trace.traceAnyA "delete token"
  Debug.Trace.traceAnyA tid
  H.fromAff $ later' 2000 slamUnit

deletePermission ∷ String → DSL Unit
deletePermission pid = do
  Debug.Trace.traceAnyA "delete permission"
  Debug.Trace.traceAnyA pid
  H.fromAff $ later' 200000 slamUnit

type AdjustedPermissions =
  { users ∷ Array Permission
  , groups ∷ Array Permission
  , tokens ∷ Array TokenPermission
  }

adjustPermissions ∷ Unit → Unit → AdjustedPermissions
adjustPermissions _ _ =
  { users:
      [ { name: "maxim@slamdata.com"
        , resume: View
        , permissionId: "999"
        , state: Just ModifyError
        }
      ]
  , groups:
      [ { name: "/foo"
        , resume: Edit
        , permissionId: "0000"
        , state: Nothing
        }
      ]
  , tokens:
      [ { name: Just "First"
        , secret: "ololo-trololo"
        , resume: Edit
        , tokenId: "1111111"
        , state: Just RevokeError
        }
      , { name: Nothing
        , secret: "foo-bar-baz-quux"
        , resume: View
        , tokenId: "22222"
        , state: Nothing
        }
      ]
  }
