module SlamData.FileSystem.Dialog.Permissions.Component
  ( comp
  , module SlamData.FileSystem.Dialog.Permissions.Component.State
  , module SlamData.FileSystem.Dialog.Permissions.Component.Query
  , module SlamData.FileSystem.Dialog.Permissions.Component.Install
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Aff (attempt)
import Control.Monad.Maybe.Trans as Mt
import Control.UI.ZClipboard as Z

import Data.Array as Arr
import Data.Functor.Coproduct.Nested (coproduct6)
import Data.NonEmpty as Ne
import Data.Lens ((.~), (?~))
import Data.Lens as L
import Data.Time (Milliseconds(..))

import DOM.HTML.Types (htmlElementToElement)

import Halogen as H
import Halogen.Component.Utils as Hu

import SlamData.Dialog.Share.Code.Component as Code
import SlamData.Dialog.Share.Confirm.Component as Confirm
import SlamData.Dialog.Share.Permissions.Component as Perms
import SlamData.Dialog.Share.User.Component as User
import SlamData.Effects (Slam)
import SlamData.FileSystem.Dialog.Permissions.Component.Install (CascadeQuery, CascadeState, ChildQuery, ChildSlot, ChildState, DSL, HTML, QueryP, RotaryQuery, RotaryState, StateP, cpCode, cpConfirm, cpGroup, cpPerms, cpRotary, cpUser)
import SlamData.FileSystem.Dialog.Permissions.Component.Query (Query(..))
import SlamData.FileSystem.Dialog.Permissions.Component.Render (render)
import SlamData.FileSystem.Dialog.Permissions.Component.State (State, ShareType(..), _canGoFurther, _confirm, _error, _groups, _permissions, _resource, _sending, _shareType, _zRef, initialState)
import SlamData.Halogen.Select.Cascade.Component as Cascade
import SlamData.Halogen.Select.Rotary.Component as Rotary
import SlamData.Quasar.Auth.Permission as Qp

import Utils.DOM (waitLoaded)

comp :: H.Component StateP QueryP Slam
comp =
  H.lifecycleParentComponent
    { render
    , eval
    , peek: Just (peek <<< H.runChildF)
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

eval :: Natural Query DSL
eval (Init next) = do
  res <- H.gets _.resource
  ps <- H.fromAff $ Qp.permissionsForResource res
  r <- H.fromEff $ newRef Nothing
  grps <- H.fromAff $ Qp.getGroups
  H.modify $ _permissions ?~ ps
  H.modify $ _zRef ?~ r
  H.modify $ _groups ?~ grps
  pure next
eval (ToConfirm next) = do
  state <- H.get
  mbPerms <- H.query' cpPerms unit $ H.request Perms.GetSelected
  mbTargets <-
    Mt.runMaybeT case state.shareType of
      Code -> empty
      Group -> do
        arr <- Mt.MaybeT $ H.query' cpGroup unit $ H.request Cascade.GetSelected
        {head, tail} <- Mt.MaybeT $ pure $ Arr.uncons arr
        pure $ Left $ head Ne.:| tail
      User -> do
        arr <- Mt.MaybeT $ H.query' cpUser unit $ H.request User.GetValues
        {head, tail} <- Mt.MaybeT $ pure $ Arr.uncons arr
        pure $ Right $ map Qp.User $ head Ne.:| tail
  H.modify
    $ L.set _confirm
    $ { resource: state.resource
      , targets: _
      , permissions: _
      }
    <$> mbTargets
    <*> mbPerms
  pure next
eval (BackToForm next) = do
  H.modify $ _confirm .~ Nothing
  H.modify $ _error .~ Nothing
  H.modify $ _canGoFurther .~ false
  pure next
eval (Share next) = do
  H.gets _.confirm
    >>= traverse_ \rq -> do
      H.modify $ _sending .~ true
      eiRes <- H.fromAff $ attempt $ Qp.doPermissionShareRequest rq
      case eiRes of
        Left e -> H.modify $ _error ?~ message e
        Right res -> Hu.sendAfter' (Milliseconds 200.0) $ H.action Dismiss
      H.modify $ _sending .~ false
  pure next
eval (Dismiss next) = pure next
eval (InitZClipboard (Just htmlEl) next) = do
  mbRef <- H.gets _.zRef
  for_ mbRef \ref -> do
    let el = htmlElementToElement htmlEl
    H.fromAff waitLoaded
    H.fromEff $
      Z.make el >>= Z.onCopy \zc ->
        readRef ref >>= traverse_ \tok ->
          Z.setData "text/plain" tok zc
  pure next
eval (InitZClipboard _ next) = pure next

peek :: forall x. ChildQuery x -> DSL Unit
peek =
  coproduct6
    userPeek
    permsPeek
    codePeek
    confirmPeek
    rotaryPeek
    groupPeek


isPermsChecked :: DSL Boolean
isPermsChecked = do
  perms <-
    (H.query' cpPerms unit $ H.request Perms.GetSelected)
      <#> fromMaybe Perms.notAllowed
  pure (perms.add || perms.read || perms.modify || perms.delete)

isGroupsSelected :: DSL Boolean
isGroupsSelected = do
  mbSelected <- H.query' cpGroup unit $ H.request Cascade.GetSelected
  pure (not $ Arr.null $ fromMaybe [ ] mbSelected)

isTokenGenerated :: DSL Boolean
isTokenGenerated = do
  mbRef <- H.gets _.zRef
  case mbRef of
    Nothing -> pure false
    Just ref -> H.fromEff $ isJust <$> readRef ref

isUsersSelected :: DSL Boolean
isUsersSelected = do
  mbUserStrings <- H.query' cpUser unit $ H.request User.GetValues
  pure (not $ Arr.null $ fromMaybe [ ] mbUserStrings)

userPeek :: forall a. User.Query a -> DSL Unit
userPeek (User.InputChanged _ _ _) = usersUpdate
userPeek (User.Remove _ _) = usersUpdate
userPeek _ = pure unit

usersUpdate :: DSL Unit
usersUpdate = do
  isP <- isPermsChecked
  isU <- isUsersSelected
  H.modify $ _canGoFurther .~ (isP && isU)

permsUpdate :: DSL Unit
permsUpdate = do
  isP <- isPermsChecked
  isU <- isUsersSelected
  isG <- isGroupsSelected
  isT <- isTokenGenerated
  tokenGenSwitch
  H.modify $ _canGoFurther .~ (isP && (isU || isG || isT))

tokenGenSwitch :: DSL Unit
tokenGenSwitch = void do
  isP <- isPermsChecked
  Hu.forceRerender'
  H.query' cpCode unit $ H.action $ Code.Toggle isP
  H.query' cpCode unit $ H.action Code.Clear

tokenUpdate :: DSL Unit
tokenUpdate = do
  isP <- isPermsChecked
  isT <- isTokenGenerated
  H.modify $ _canGoFurther .~ (isP && isT)

groupsUpdate :: DSL Unit
groupsUpdate = do
  isP <- isPermsChecked
  isG <- isGroupsSelected
  H.modify $ _canGoFurther .~ (isP && isG)

permsPeek :: forall a. Perms.Query a -> DSL Unit
permsPeek (Perms.ToggleAdd _) = permsUpdate
permsPeek (Perms.ToggleRead _) = permsUpdate
permsPeek (Perms.ToggleModify _) = permsUpdate
permsPeek (Perms.ToggleDelete _) = permsUpdate
permsPeek _ = pure unit


codePeek :: forall a. Code.Query a -> DSL Unit
codePeek (Code.Generate _) = unit <$ Mt.runMaybeT do
  ref <- Mt.MaybeT $ H.gets _.zRef
  token <-
    Mt.MaybeT $ map join $ H.query' cpCode unit $ H.request Code.GetPermissionToken
  H.fromEff $ writeRef ref $ Just $ Qp.runPermissionToken token
  lift $ tokenUpdate
codePeek (Code.Clear _) = do
  H.gets _.zRef >>= traverse_ (H.fromEff <<< flip writeRef Nothing)
  tokenUpdate
codePeek _ = pure unit

confirmPeek :: forall a. Confirm.Query a -> DSL Unit
confirmPeek _ = pure unit

rotaryPeek :: forall a. RotaryQuery a -> DSL Unit
rotaryPeek (Rotary.Selected opt _) = do
  H.modify $ _shareType .~ (Rotary.runOption opt).shareType
  H.modify $ _canGoFurther .~ false
  tokenGenSwitch
rotaryPeek _ = pure unit

groupPeek :: forall a. CascadeQuery a -> DSL Unit
groupPeek (Cascade.Selected _ _ _) = groupsUpdate
groupPeek (Cascade.Remove _ _) = groupsUpdate
groupPeek _ = pure unit
