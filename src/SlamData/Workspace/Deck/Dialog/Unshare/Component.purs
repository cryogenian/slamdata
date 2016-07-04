module SlamData.Workspace.Deck.Dialog.Unshare.Component where

import SlamData.Prelude

import Control.UI.Browser (select)
import Control.UI.ZClipboard as Z

import Data.Array as Arr
import Data.Foldable as F
import Data.Lens (LensP, lens, (.~), (%~), (?~))
import Data.Lens.Index (ix)
import Data.Set as Set
import Data.Map as Map
import Data.StrMap as SM
import Data.Path.Pathy as Pt

import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as Cp

import Quasar.Advanced.Types as QTA

import SlamData.Effects (Slam)
import SlamData.Render.Common (glyph)
import SlamData.Quasar.Security as Q
import SlamData.Workspace.Deck.Dialog.Share.Model (ShareResume(..), printShareResume)
import SlamData.Workspace.Deck.Dialog.Share.Model as Model

import Utils.Path (DirPath, parseFilePath)

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

type PermissionId = Set.Set QTA.PermissionId

data PermissionState
  = Unsharing
  | Modifying
  | RevokeError
  | ModifyError

derive instance permissionStateEq ∷ Eq PermissionState

type Permission =
  { resume ∷ ShareResume
  , actions ∷ Map.Map QTA.PermissionId QTA.ActionR
  , state ∷ Maybe PermissionState
  }


data Modifyable = User | Group

type TokenPermission =
  { name ∷ Maybe String
  , secret ∷ String
  , resume ∷ ShareResume
  , tokenId ∷ QTA.TokenId
  , state ∷ Maybe PermissionState
  }

_resume ∷ ∀ a r. LensP {resume ∷ a|r} a
_resume = lens (_.resume) (_{resume = _})

_tokenId ∷ ∀ a r. LensP {tokenId ∷ a|r} a
_tokenId = lens (_.tokenId) (_{tokenId = _})

_secret ∷ ∀ a r. LensP {secret ∷ a|r} a
_secret = lens (_.secret) (_{secret = _})

-- :( `ix` doesn't work without `Maybe PermissionState`
_state ∷ ∀ r. LensP {state ∷ (Maybe PermissionState)|r} (Maybe PermissionState)
_state = lens (_.state) (_{state = _})

type State =
  { userPermissions ∷ SM.StrMap Permission
  , tokenPermissions ∷ Array TokenPermission
  , groupPermissions ∷ SM.StrMap Permission
  , loading ∷ Boolean
  , errored ∷ Boolean
  , deckPath ∷ DirPath
  }

initialState ∷ DirPath → State
initialState deckPath =
  { userPermissions: SM.empty
  , tokenPermissions: [ ]
  , groupPermissions: SM.empty
  , loading: true
  , errored: false
  , deckPath
  }

_userPermissions ∷ ∀ a r. LensP {userPermissions ∷ a|r} a
_userPermissions = lens (_.userPermissions) (_{userPermissions = _})

_groupPermissions ∷ ∀ a r. LensP {groupPermissions ∷ a |r} a
_groupPermissions = lens (_.groupPermissions) (_{groupPermissions = _})

_tokenPermissions ∷ ∀ a r. LensP {tokenPermissions ∷ a|r} a
_tokenPermissions = lens (_.tokenPermissions) (_{tokenPermissions = _})

_loading ∷ ∀ a r. LensP {loading ∷ a|r} a
_loading = lens (_.loading) (_{loading = _})

_errored ∷ ∀ a r. LensP {errored ∷ a|r} a
_errored = lens (_.errored) (_{errored = _})

data Query a
  = Dismiss a
  | InitZClipboard String (Maybe HTMLElement) a
  | Init a
  | SelectElement HTMLElement a
  | PermissionResumeChanged String String a
  | Unshare String a
  | UnshareToken QTA.TokenId  a


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
            $ (if SM.isEmpty state.userPermissions
                 then [ ]
                 else
                 [ HH.label
                     [ HP.classes [ HH.className "subject-label" ] ]
                     [ HH.text "Users" ]
                 ]
                 ⊕ (foldMap renderUserOrGroup $ SM.toList state.userPermissions)
              )
            ⊕ (if SM.isEmpty state.groupPermissions
                 then [ ]
                 else
                 [ HH.label
                    [ HP.classes [ HH.className "subject-label" ] ]
                    [ HH.text "Groups" ]
                 ]
                 ⊕ (foldMap renderUserOrGroup $ SM.toList state.groupPermissions)
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
            ⊕ (if SM.isEmpty state.userPermissions
                  ∧ SM.isEmpty state.groupPermissions
                  ∧ Arr.null state.tokenPermissions
                 then
                   [ HH.p_ [ HH.text "This deck has no shared permissions" ] ]
                 else [ ]
              )
        ]
    , let
        states =
          (foldMap (\x → [x.state]) state.userPermissions)
          ⊕ (foldMap (\x → [x.state]) state.groupPermissions)
          ⊕ (foldMap (\x → [x.state]) state.tokenPermissions)
        somethingErrored =
          F.any (\x → x ≡ Just ModifyError ∨ x ≡ Just RevokeError) states
        somethingHappening =
          F.any (\x → x ≡ Just Unsharing ∨ x ≡ Just Modifying) states
      in
       HH.div
        [ HP.classes
            $ [ HH.className "deck-dialog-footer" ]
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
        , HH.div
            [ HP.classes
                $ [ B.alert, B.alertDanger ]
                ⊕ (if state.errored then [ ] else [ B.hidden ])
            ]
            [ HH.text
                $ "Couldn't share/unshare deck. "
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

renderUserOrGroup ∷ (String × Permission) → Array HTML
renderUserOrGroup (name × perm) =
  [ HH.div
    [ HP.classes [ B.row ] ]
    [ HH.div
        [ HP.classes [ B.colXs7 ] ]
        [ HH.text name ]
    , HH.div
        [ HP.classes
            $ [ B.colXs3 ]
            ⊕ if perm.state ≡ Just ModifyError then [ B.hasError ] else [ ]
        ]
        [ HH.select
            [ HP.classes [ B.formControl ]
            , HE.onValueChange (HE.input (PermissionResumeChanged name))
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
            , HE.onClick (HE.input_ (Unshare name))
            , HP.disabled $ perm.state ≡ Just Modifying ∨ perm.state ≡ Just Unsharing
            ]
            [ HH.text if perm.state ≡ Just Unsharing
                        then "Revoking..."
                        else "Unshare"
            ]
        ]
    ]
  ]

renderToken ∷ TokenPermission → HTML
renderToken token =
  HH.div
    [ HP.classes [ B.row ] ]
    [ HH.div
        [ HP.classes [ B.colXs4, HH.className "token-name-field" ] ]
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
  tokensRes ← Q.tokenList
  deckPath ← H.gets _.deckPath
  case tokensRes of
    Left e → H.modify (_{errored = true})
    Right toks →
      let
        tokenPerms = prepareAndFilterTokens toks deckPath
      in
        H.modify (_{tokenPermissions = tokenPerms})

  permsRes ← Q.permissionList false
  case permsRes of
    Left e → H.modify (_{errored = true})
    Right ps →
      let
        adjusted = adjustPermissions ps deckPath
      in
        H.modify (_{ userPermissions = adjusted.users
                   , groupPermissions = adjusted.groups
                   })
  H.modify (_{ loading = false})
eval (InitZClipboard token mbEl next) =
  next <$ for_ mbEl \el → do
    H.fromEff $ Z.make (htmlElementToElement el)
      >>= Z.onCopy (Z.setData "text/plain" token)
eval (Dismiss next) = pure next
eval (SelectElement el next) =
  next <$ H.fromEff (select el)
eval (PermissionResumeChanged name string next) = next <$ do
  state ← H.get
  let
    mbUserPerms = SM.lookup name state.userPermissions
    mbGroupPerms = SM.lookup name state.groupPermissions

    newResume | string ≡ "view" = View
              | otherwise = Edit

  for_ mbUserPerms \perms → do
    H.modify
      $ _userPermissions ∘ ix name ∘ _state ?~ Modifying
    changePermissionResumeForUser name state.deckPath newResume perms
    H.modify
      $ (_userPermissions ∘ ix name ∘ _resume .~ newResume)
      ∘ (_userPermissions ∘ ix name ∘ _state .~ Nothing)

  for_ mbGroupPerms \perms → do
    H.modify
      $ _groupPermissions ∘ ix name ∘ _state ?~ Modifying
    changePermissionResumeForGroup name state.deckPath newResume perms
    H.modify
      $ (_groupPermissions ∘ ix name ∘ _resume .~ newResume)
      ∘ (_groupPermissions ∘ ix name ∘ _state .~ Nothing)


eval (UnshareToken tokenId next) = next <$ do
  state ← H.get
  let
    mtix = Arr.findIndex (\x → x.tokenId ≡ tokenId) state.tokenPermissions

  for_ mtix \tix → do
    H.modify $ _tokenPermissions ∘ ix tix ∘ _state ?~ Unsharing

    Q.deleteToken tokenId >>= case _ of
      Left _ →
        H.modify
          $ (_tokenPermissions ∘ ix tix ∘ _state ?~ RevokeError)
          ∘ (_errored .~ false)

      Right _ →
        H.modify
          $ _tokenPermissions %~ Arr.filter (\x → x.tokenId ≠ tokenId)

eval (Unshare name next) = next <$ do
  state ← H.get
  let
    mbUserPerms = SM.lookup name state.userPermissions
    mbGroupPerms = SM.lookup name state.groupPermissions

  for_ mbUserPerms \userPerms → do
    H.modify $ _userPermissions ∘ ix name ∘ _state ?~ Unsharing
    deletePermission userPerms.actions
    H.modify
      $ (_userPermissions %~ SM.delete name)
      ∘ (_userPermissions ∘ ix name ∘ _state .~ Nothing)

  for_ mbGroupPerms \groupPerms → do
    H.modify $ _groupPermissions ∘ ix name ∘ _state ?~ Unsharing
    deletePermission groupPerms.actions
    H.modify
      $ (_groupPermissions %~ SM.delete name)
      ∘ (_groupPermissions ∘ ix name ∘ _state .~ Nothing)

slamUnit ∷ Slam Unit
slamUnit = pure unit

changePermissionResumeForUser
  ∷ String → DirPath → ShareResume → Permission → DSL Unit
changePermissionResumeForUser name deckPath res perm = do
  deletePermission perm.actions
  let
    actions = Model.sharingActions deckPath res
    shareRequest =
      { users: [ QTA.UserId name ]
      , groups: [ ]
      , actions
      }

  Q.sharePermission shareRequest
  pure unit

changePermissionResumeForGroup
  ∷ String → DirPath → ShareResume → Permission → DSL Unit
changePermissionResumeForGroup name deckPath res perm = do
  deletePermission perm.actions
  let
    actions = Model.sharingActions deckPath res
    shareRequest =
      parseFilePath name
      <#> \x → { users: [ ]
               , groups: [ x ]
               , actions
               }

  for_ shareRequest \x → do
    Q.sharePermission x
    pure unit

deletePermission ∷ Map.Map QTA.PermissionId QTA.ActionR → DSL Unit
deletePermission permissionMap = do
  results ←
    for (Map.keys permissionMap) Q.deletePermission

  if F.any isLeft results
    then Debug.Trace.traceAnyA "errored"
    else Debug.Trace.traceAnyA "ok"

type AdjustedPermissions =
  { users ∷ SM.StrMap Permission
  , groups ∷ SM.StrMap Permission
  }

adjustPermissions ∷ Array QTA.PermissionR → DirPath → AdjustedPermissions
adjustPermissions prs deckPath =
  let
    folded = foldl foldFn Map.empty prs
    foldFn mp pr =
      Map.alter (alterFn pr) pr.grantedTo mp

    alterFn pr Nothing = Just [ pr ]
    alterFn pr (Just arr) = Just $ Arr.cons pr arr

    sharingActionsEdit =
      Set.fromFoldable $ map QTA.Action $ Model.sharingActions deckPath Edit
    sharingActionsView =
      Set.fromFoldable $ map QTA.Action $ Model.sharingActions deckPath View


    obj = foldl objFoldFn { users: SM.empty, groups: SM.empty } $ Map.toList folded

    objFoldFn acc@{ users, groups } (key × arr) =
      let
        actions = foldMap (\a → Map.singleton a.id a.action) arr
        actionsSet = Set.fromFoldable $ map QTA.Action actions
        perm = { resume: View
               , actions
               , state: Nothing
               }
      in case key of
        QTA.TokenGranted _ → acc
        QTA.UserGranted (QTA.UserId uid) →
          let
            res
              | sharingActionsEdit ≡ actionsSet =
                acc{users = SM.insert uid perm{resume = Edit} users }
              | sharingActionsView ≡ actionsSet =
                acc{users = SM.insert uid perm users}
              | otherwise = acc
          in res
        QTA.GroupGranted pt →
          let
            gid = Pt.printPath pt
            res
              | sharingActionsEdit ≡ actionsSet =
                acc{groups = SM.insert gid perm{resume = Edit} groups }
              | sharingActionsView ≡ actionsSet =
                acc{groups = SM.insert gid perm groups}
              | otherwise = acc
          in res
  in obj


prepareAndFilterTokens ∷ Array QTA.TokenR → DirPath →  Array TokenPermission
prepareAndFilterTokens toks deckPath =
  foldMap foldFn toks
  where
  foldFn ∷ QTA.TokenR → Array TokenPermission
  foldFn tr =
    let
      sharingActionsEdit =
        Set.fromFoldable $ map QTA.Action $ Model.sharingActions deckPath Edit
      sharingActionsView =
        Set.fromFoldable $ map QTA.Action $ Model.sharingActions deckPath View
      actions =
        Set.fromFoldable $ map QTA.Action tr.actions
      preToken =
        { name: map QTA.runTokenName tr.name
        , secret: maybe "" QTA.runTokenHash tr.secret
        , tokenId: tr.id
        , state: Nothing
        , resume: View
        }
      res
        | sharingActionsEdit ≡ actions =
          [ preToken{resume = Edit} ]
        | sharingActionsView ≡ actions =
          [ preToken ]
        | otherwise =
            [ ]
    in
      res
