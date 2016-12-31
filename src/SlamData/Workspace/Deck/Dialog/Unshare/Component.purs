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

module SlamData.Workspace.Deck.Dialog.Unshare.Component where

import SlamData.Prelude

import Control.Monad.Aff.AVar (makeVar, takeVar, putVar)
import Control.Monad.Eff.Ref (newRef, modifyRef, readRef)
import Control.UI.Browser (select)

import Data.Array as Arr
import Data.Foldable as F
import Data.Lens (Lens', lens, (.~), (%~), (?~))
import Data.Lens.Index (ix)
import Data.List as L
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

import SlamData.Monad (Slam)
import SlamData.Quasar.Security as Q
import SlamData.Render.Common (glyph)
import SlamData.Workspace.Deck.Dialog.Share.Model (ShareResume(..), printShareResume)
import SlamData.Workspace.Deck.Dialog.Share.Model as Model

import Utils.Path (parseFilePath)

import ZClipboard as Z

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

_resume ∷ ∀ a r. Lens' {resume ∷ a|r} a
_resume = lens (_.resume) (_{resume = _})

_tokenId ∷ ∀ a r. Lens' {tokenId ∷ a|r} a
_tokenId = lens (_.tokenId) (_{tokenId = _})

_secret ∷ ∀ a r. Lens' {secret ∷ a|r} a
_secret = lens (_.secret) (_{secret = _})

-- :( `ix` doesn't work without `Maybe PermissionState`
_state ∷ ∀ r. Lens' {state ∷ (Maybe PermissionState)|r} (Maybe PermissionState)
_state = lens (_.state) (_{state = _})

_actions
  ∷ ∀ r
  . Lens'
      {actions ∷ Map.Map QTA.PermissionId QTA.ActionR|r}
      (Map.Map QTA.PermissionId QTA.ActionR)
_actions = lens (_.actions) (_{actions = _})

type State =
  { userPermissions ∷ SM.StrMap Permission
  , tokenPermissions ∷ Array TokenPermission
  , groupPermissions ∷ SM.StrMap Permission
  , loading ∷ Boolean
  , errored ∷ Boolean
  , sharingInput ∷ Model.SharingInput
  }

initialState ∷ Model.SharingInput → State
initialState sharingInput =
  { userPermissions: SM.empty
  , tokenPermissions: [ ]
  , groupPermissions: SM.empty
  , loading: true
  , errored: false
  , sharingInput
  }

_userPermissions ∷ ∀ a r. Lens' {userPermissions ∷ a|r} a
_userPermissions = lens (_.userPermissions) (_{userPermissions = _})

_groupPermissions ∷ ∀ a r. Lens' {groupPermissions ∷ a |r} a
_groupPermissions = lens (_.groupPermissions) (_{groupPermissions = _})

_tokenPermissions ∷ ∀ a r. Lens' {tokenPermissions ∷ a|r} a
_tokenPermissions = lens (_.tokenPermissions) (_{tokenPermissions = _})

_loading ∷ ∀ a r. Lens' {loading ∷ a|r} a
_loading = lens (_.loading) (_{loading = _})

_errored ∷ ∀ a r. Lens' {errored ∷ a|r} a
_errored = lens (_.errored) (_{errored = _})

data Query a
  = Dismiss a
  | InitZClipboard String (Maybe HTMLElement) a
  | Init a
  | SelectElement HTMLElement a
  | PermissionResumeChanged String String a
  | Unshare String a
  | UnshareToken QTA.TokenId  a


type AdjustedPermissions =
  { users ∷ SM.StrMap Permission
  , groups ∷ SM.StrMap Permission
  }

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
  HH.div
    [ HP.classes [ HH.className "deck-dialog-unshare" ] ]
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
                 then
                   [ ]
                 else
                   [ HH.h5_ [ HH.text "Users" ] ]
                   ⊕ (foldMap renderUserOrGroup $ SM.toList state.userPermissions)
              )
            ⊕ (if SM.isEmpty state.groupPermissions
                 then
                   [ ]
                 else
                   [ HH.h5_ [ HH.text "Groups" ] ]
                   ⊕ (foldMap renderUserOrGroup $ SM.toList state.groupPermissions)
              )
            ⊕ (if Arr.null state.tokenPermissions
                 then
                   [ ]
                 else
                   [ HH.h5_ [ HH.text "Tokens" ] ]
                   ⊕ map renderToken state.tokenPermissions
              )
            ⊕ (if SM.isEmpty state.userPermissions
                  ∧ SM.isEmpty state.groupPermissions
                  ∧ Arr.null state.tokenPermissions
                  ∧ not state.errored
                 then
                   [ HH.div_
                       [ HH.text "This deck hasn't been shared with anyone." ]
                   ]
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
                ⊕ (if state.errored ∨ somethingErrored then [ ] else [ B.hidden ])
            ]
            [ HH.text
                $ if state.errored
                  then
                    "Sharing information is unavailable. To access or change the "
                    ⊕ "sharing information for this deck please check your network connection "
                    ⊕ "and try again."

                  else
                    "This action couldn't be performed. "
                    ⊕ "Please check your network connection and try again."

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
      [ HP.class_ $ HH.className "sd-unshare-subject" ]
      [ HH.label_ [ HH.text name ]
      , HH.div
          [ HP.classes $ if perm.state ≡ Just ModifyError then [ B.hasError ] else [ ] ]
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
          [ HP.classes $ if perm.state ≡ Just RevokeError then [ B.hasError ] else [ ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnPrimary ]
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
    [ HP.class_ $ HH.className "sd-unshare-subject" ]
    [ HH.label_
        [ HH.text $ fromMaybe "Untitled token" token.name
        ]
    , HH.span
        [ HP.classes [ HH.className "sd-unshare-subject-access-type" ] ]
        [ HH.text $ printShareResume token.resume ]
    , HH.div
        [ HP.classes
            $ [ HH.className "sd-url" ]
            ⊕ if token.state ≡ Just ModifyError then [ B.hasError ] else [ ]
        ]
        [ HH.input
            [ HP.readonly true
            , HP.title "Token secret"
            , HP.value token.secret
            , HE.onClick $ HE.input (SelectElement ∘ _.target)
            , HP.disabled $ isJust token.state
        ]
        , HH.button
            [ HP.classes [ B.btn, B.btnDefault ]
            , HP.ref (H.action ∘ InitZClipboard token.secret)
            , HP.disabled $ token.state ≡ Just Modifying ∨ token.state ≡ Just Unsharing
            ]
            [ glyph B.glyphiconCopy ]
        ]
    , HH.div
        [ HP.classes $ if token.state ≡ Just RevokeError then [ B.hasError ] else [ ] ]
        [ HH.div
            [ HP.class_ B.btnGroup ]
            [ HH.button
                [ HP.classes [ B.btn, B.btnPrimary ]
                , HE.onClick (HE.input_ (UnshareToken token.tokenId))
                , HP.disabled $ token.state ≡ Just Modifying ∨ token.state ≡ Just Unsharing
                ]
                [ HH.text if token.state ≡ Just Unsharing
                            then "Revoking..."
                            else "Unshare"
                ]
            ]
        ]
  ]


eval ∷ Query ~> DSL
eval (Init next) = next <$ do
  tokensRes ← Q.tokenList
  sharingInput ← H.gets _.sharingInput
  case tokensRes of
    Left e → H.modify (_{errored = true})
    Right toks →
      let
        tokenPerms = prepareAndFilterTokens toks sharingInput
      in
        H.modify (_{tokenPermissions = tokenPerms})

  permsRes ← Q.permissionList false
  case permsRes of
    Left e → H.modify (_{errored = true})
    Right ps →
      let
        adjusted = adjustPermissions ps sharingInput
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
  H.modify (_{errored = false})
  let
    mbUserPerms = SM.lookup name state.userPermissions
    mbGroupPerms = SM.lookup name state.groupPermissions

    newResume | string ≡ "view" = View
              | otherwise = Edit

    oldResume | string ≡ "view" = Edit
              | otherwise = View

  for_ mbUserPerms \perms → do
    H.modify
      $ (_userPermissions ∘ ix name ∘ _state ?~ Modifying)
      ∘ (_userPermissions ∘ ix name ∘ _resume .~ newResume)
    (succeeded × actionMap) ←
      changePermissionResumeForUser name state.sharingInput newResume perms
    H.modify
      $ (_userPermissions ∘ ix name ∘ _actions .~ actionMap)
      ∘ if succeeded
        then
          (_userPermissions ∘ ix name ∘ _state .~ Nothing)
        else
          (_userPermissions ∘ ix name ∘ _state ?~ ModifyError)
          ∘ (_userPermissions ∘ ix name ∘ _resume .~ oldResume)

  for_ mbGroupPerms \perms → do
    H.modify
      $ (_groupPermissions ∘ ix name ∘ _state ?~ Modifying)
      ∘ (_groupPermissions ∘ ix name ∘ _resume .~ newResume)

    (succeeded × actionMap)  ←
      changePermissionResumeForGroup name state.sharingInput newResume perms
    H.modify
      $ (_groupPermissions ∘ ix name ∘ _actions .~ actionMap)
      ∘ if succeeded
        then
          (_groupPermissions ∘ ix name ∘ _state .~ Nothing)
        else
          (_groupPermissions ∘ ix name ∘ _state ?~ ModifyError)
          ∘ (_groupPermissions ∘ ix name ∘ _resume .~ oldResume)


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
  H.modify (_{errored = false})
  let
    mbUserPerms = SM.lookup name state.userPermissions
    mbGroupPerms = SM.lookup name state.groupPermissions

  for_ mbUserPerms \userPerms → do
    H.modify $ _userPermissions ∘ ix name ∘ _state ?~ Unsharing
    leftPids ← deletePermission userPerms.actions
    H.modify
      if Map.isEmpty leftPids
        then
        (_userPermissions %~ SM.delete name)
        ∘ (_userPermissions ∘ ix name ∘ _state .~ Nothing)
        else
        (_userPermissions ∘ ix name ∘ _actions .~ leftPids)
        ∘ (_userPermissions ∘ ix name ∘ _state ?~ RevokeError)


  for_ mbGroupPerms \groupPerms → do
    H.modify $ _groupPermissions ∘ ix name ∘ _state ?~ Unsharing
    leftPids ← deletePermission groupPerms.actions
    H.modify
      if Map.isEmpty leftPids
        then
        (_groupPermissions ∘ ix name ∘ _state .~ Nothing)
        ∘ (_groupPermissions %~ SM.delete name)
        else
        (_groupPermissions ∘ ix name ∘ _actions .~ leftPids)
        ∘ (_groupPermissions ∘ ix name ∘ _state ?~ RevokeError)


changePermissionResumeForUser
  ∷ String
  → Model.SharingInput
  → ShareResume
  → Permission
  → DSL (Boolean × (Map.Map QTA.PermissionId QTA.ActionR))
changePermissionResumeForUser name sharingInput res perm = do
  leftPids ← deletePermission perm.actions
  if not $ Map.isEmpty leftPids
    then
    pure $ false × leftPids
    else do
    let
      actions = Model.sharingActions sharingInput res
      shareRequest =
        { users: [ QTA.UserId name ]
        , groups: [ ]
        , actions
        }
    shareRes ← Q.sharePermission shareRequest
    case shareRes of
      Left _ → pure $ false × Map.empty
      Right ps → pure $ true × foldMap (\p → Map.singleton p.id p.action) ps


changePermissionResumeForGroup
  ∷ String
  → Model.SharingInput
  → ShareResume
  → Permission
  → DSL (Boolean × (Map.Map QTA.PermissionId QTA.ActionR))
changePermissionResumeForGroup name sharingInput res perm =
  case parseFilePath name of
    Nothing → pure $ false × perm.actions
    Just groupPath → do
      leftPids ← deletePermission perm.actions
      if not $ Map.isEmpty leftPids
        then
        pure $ false × leftPids
        else do
        let
          actions = Model.sharingActions sharingInput res
          shareRequest =
            { users: [ ]
            , groups: [ Right groupPath ]
            , actions
            }
        shareRes ← Q.sharePermission shareRequest
        case shareRes of
          Left _ → pure $ false × Map.empty
          Right ps → pure $ true × foldMap (\p → Map.singleton p.id p.action) ps

deletePermission
  ∷ Map.Map QTA.PermissionId QTA.ActionR
  → DSL (Map.Map QTA.PermissionId QTA.ActionR)
deletePermission permissionMap = do
  r ← H.fromEff $ newRef $ Map.empty × Map.size permissionMap
  resultVar ← H.fromAff makeVar
  H.liftH $ parTraverse (go r resultVar) (Map.toList permissionMap)
  H.fromAff $ takeVar resultVar
  where
  go r resultVar (pid × act) = do
    result ← Q.deletePermission pid
    H.fromEff $ modifyRef r \(acc × count) → acc × (count - 1)
    case result of
      Left _ → H.fromEff $ modifyRef r \(acc × count) → (Map.insert pid act acc) × count
      Right _ → pure unit
    (acc × count) ← H.fromEff $ readRef r
    when (count ≡ 0) $ H.fromAff $ putVar resultVar acc


adjustPermissions ∷ Array QTA.PermissionR → Model.SharingInput → AdjustedPermissions
adjustPermissions prs sharingInput =
  let
    folded = foldl foldFn Map.empty prs
    foldFn mp pr =
      Map.alter (alterFn pr) pr.grantedTo mp

    alterFn pr Nothing = Just [ pr ]
    alterFn pr (Just arr) = Just $ Arr.cons pr arr

    sharingActionsEdit =
      Set.fromFoldable $ map QTA.Action $ Model.sharingActions sharingInput Edit
    sharingActionsView =
      Set.fromFoldable $ map QTA.Action $ Model.sharingActions sharingInput View


    obj = foldl objFoldFn { users: SM.empty, groups: SM.empty } $ Map.toList folded

    -- Set.subset is sooooo slow :( @crygoenian
    subset ∷ ∀ a. Ord a ⇒ Set.Set a → Set.Set a → Boolean
    subset needle hay =
      let
        needleLst = L.sort $ L.fromFoldable needle
        hayLst = L.sort $ L.fromFoldable hay

        subset' ∷ L.List a → L.List a → Boolean
        subset' L.Nil _ = true
        subset' _ L.Nil = false
        subset' needle'@(L.Cons x xs) (L.Cons y ys) =
          if x ≡ y
          then subset' xs ys
          else subset' needle' ys
      in subset' needleLst hayLst


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
              | subset sharingActionsEdit actionsSet =
                acc{users = SM.insert uid perm{resume = Edit} users }
              | subset sharingActionsView actionsSet =
                acc{users = SM.insert uid perm users}
              | otherwise = acc
          in res
        QTA.GroupGranted pt →
          let
            gid = either QTA.printRoot Pt.printPath pt
            res
              | subset sharingActionsEdit actionsSet =
                acc{groups = SM.insert gid perm{resume = Edit} groups }
              | subset sharingActionsView actionsSet =
                acc{groups = SM.insert gid perm groups}
              | otherwise = acc
          in res
  in obj


prepareAndFilterTokens ∷ Array QTA.TokenR → Model.SharingInput →  Array TokenPermission
prepareAndFilterTokens toks sharingInput =
  foldMap foldFn toks
  where
  foldFn ∷ QTA.TokenR → Array TokenPermission
  foldFn tr =
    let
      sharingActionsEdit =
        Set.fromFoldable $ map QTA.Action $ Model.sharingActions sharingInput Edit
      sharingActionsView =
        Set.fromFoldable $ map QTA.Action $ Model.sharingActions sharingInput View
      actions =
        Set.fromFoldable $ map QTA.Action tr.actions
      preToken =
        { name: map QTA.runTokenName tr.name
        , secret: QTA.runTokenHash tr.secret
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
