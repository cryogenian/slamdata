module SlamData.FileSystem.Dialog.Permissions.Component
  (
    comp
  , module SlamData.FileSystem.Dialog.Permissions.Component.State
  , module SlamData.FileSystem.Dialog.Permissions.Component.Query
  , module SlamData.FileSystem.Dialog.Permissions.Component.Install
  ) where

import Prelude

import Control.Bind (join)
import Control.Plus (empty)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Aff (attempt)
import Control.Monad.Maybe.Trans as Mt
import Control.Monad.Trans (lift)
import Control.UI.ZClipboard as Z

import Data.Array as Arr
import Data.Either as E
import Data.Functor ((<$))
import Data.Functor.Coproduct.Nested (coproduct6)
import Data.Maybe as M
import Data.NonEmpty as Ne
import Data.Lens ((.~), (?~))
import Data.Lens as L
import Data.Functor.Aff (liftAff)
import Data.Functor.Eff (liftEff)
import Data.Foldable as F
import Data.Time (Milliseconds(..))

import DOM.HTML.Types (htmlElementToElement)

import Halogen hiding (HTML())
import Halogen.Component.Utils as Hu

import SlamData.Effects (Slam())
import SlamData.Dialog.Share.User.Component as User
import SlamData.Dialog.Share.Code.Component as Code
import SlamData.Dialog.Share.Confirm.Component as Confirm
import SlamData.Dialog.Share.Permissions.Component as Perms
import SlamData.Halogen.Select.Rotary.Component as Rotary
import SlamData.Halogen.Select.Cascade.Component as Cascade

import SlamData.FileSystem.Dialog.Permissions.Component.State
import SlamData.FileSystem.Dialog.Permissions.Component.Query
import SlamData.FileSystem.Dialog.Permissions.Component.Install
import SlamData.FileSystem.Dialog.Permissions.Component.Render

import Quasar.Auth.Permission as Qp

import Utils.DOM (waitLoaded)

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval (peek <<< runChildF)

eval :: Natural Query DSL
eval (Init next) = do
  res <- gets _.resource
  ps <- liftAff $ Qp.permissionsForResource res
  r <- liftEff $ newRef M.Nothing
  grps <- liftAff $ Qp.getGroups
  modify $ _permissions ?~ ps
  modify $ _zRef ?~ r
  modify $ _groups ?~ grps
  pure next
eval (ToConfirm next) = do
  state <- get
  mbPerms <- query' cpPerms unit $ request Perms.GetSelected
  mbTargets <-
    Mt.runMaybeT case state.shareType of
      Code -> empty
      Group -> do
        arr <- Mt.MaybeT $ query' cpGroup unit $ request Cascade.GetSelected
        {head, tail} <- Mt.MaybeT $ pure $ Arr.uncons arr
        pure $ E.Left $ head Ne.:| tail
      User -> do
        arr <- Mt.MaybeT $ query' cpUser unit $ request User.GetValues
        {head, tail} <- Mt.MaybeT $ pure $ Arr.uncons arr
        pure $ E.Right $ map Qp.User $ head Ne.:| tail
  modify
    $ L.set _confirm
    $ { resource: state.resource
      , targets: _
      , permissions: _
      }
    <$> mbTargets
    <*> mbPerms
  pure next
eval (BackToForm next) = do
  modify $ _confirm .~ M.Nothing
  modify $ _error .~ M.Nothing
  modify $ _canGoFurther .~ false
  pure next
eval (Share next) = do
  gets _.confirm
    >>= F.traverse_ \rq -> do
      modify $ _sending .~ true
      eiRes <- liftAff $ attempt $ Qp.doPermissionShareRequest rq
      case eiRes of
        E.Left e -> modify $ _error ?~ message e
        E.Right res -> Hu.sendAfter' (Milliseconds 200.0) $ action Dismiss
      modify $ _sending .~ false
  pure next
eval (Dismiss next) = pure next
eval (InitZClipboard htmlEl next) = do
  mbRef <- gets _.zRef
  F.for_ mbRef \ref -> do
    let el = htmlElementToElement htmlEl
    liftAff waitLoaded
    liftEff $
      Z.make el >>= Z.onCopy \zc ->
        readRef ref >>= F.traverse_ \tok ->
          Z.setData "text/plain" tok zc
  pure next

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
    (query' cpPerms unit $ request Perms.GetSelected)
      <#> M.fromMaybe Perms.notAllowed
  pure (perms.add || perms.read || perms.modify || perms.delete)

isGroupsSelected :: DSL Boolean
isGroupsSelected = do
  mbSelected <- query' cpGroup unit $ request Cascade.GetSelected
  pure (not $ Arr.null $ M.fromMaybe [ ] mbSelected)

isTokenGenerated :: DSL Boolean
isTokenGenerated = do
  mbRef <- gets _.zRef
  case mbRef of
    M.Nothing -> pure false
    M.Just ref -> liftEff $ M.isJust <$> readRef ref

isUsersSelected :: DSL Boolean
isUsersSelected = do
  mbUserStrings <- query' cpUser unit $ request User.GetValues
  pure (not $ Arr.null $ M.fromMaybe [ ] mbUserStrings)

userPeek :: forall a. User.Query a -> DSL Unit
userPeek (User.InputChanged _ _ _) = usersUpdate
userPeek (User.Remove _ _) = usersUpdate
userPeek _ = pure unit

usersUpdate :: DSL Unit
usersUpdate = do
  isP <- isPermsChecked
  isU <- isUsersSelected
  modify $ _canGoFurther .~ (isP && isU)

permsUpdate :: DSL Unit
permsUpdate = do
  isP <- isPermsChecked
  isU <- isUsersSelected
  isG <- isGroupsSelected
  isT <- isTokenGenerated
  tokenGenSwitch
  modify $ _canGoFurther .~ (isP && (isU || isG || isT))

tokenGenSwitch :: DSL Unit
tokenGenSwitch = void do
  isP <- isPermsChecked
  Hu.forceRerender'
  query' cpCode unit $ action $ Code.Toggle isP
  query' cpCode unit $ action Code.Clear

tokenUpdate :: DSL Unit
tokenUpdate = do
  isP <- isPermsChecked
  isT <- isTokenGenerated
  modify $ _canGoFurther .~ (isP && isT)

groupsUpdate :: DSL Unit
groupsUpdate = do
  isP <- isPermsChecked
  isG <- isGroupsSelected
  modify $ _canGoFurther .~ (isP && isG)

permsPeek :: forall a. Perms.Query a -> DSL Unit
permsPeek (Perms.ToggleAdd _) = permsUpdate
permsPeek (Perms.ToggleRead _) = permsUpdate
permsPeek (Perms.ToggleModify _) = permsUpdate
permsPeek (Perms.ToggleDelete _) = permsUpdate
permsPeek _ = pure unit


codePeek :: forall a. Code.Query a -> DSL Unit
codePeek (Code.Generate _) = unit <$ Mt.runMaybeT do
  ref <- Mt.MaybeT $ gets _.zRef
  token <-
    Mt.MaybeT $ map join $ query' cpCode unit $ request Code.GetPermissionToken
  liftEff $ writeRef ref $ M.Just $ Qp.runPermissionToken token
  lift $ tokenUpdate
codePeek (Code.Clear _) = do
  gets _.zRef >>= F.traverse_ (liftEff <<< flip writeRef M.Nothing)
  tokenUpdate
codePeek _ = pure unit

confirmPeek :: forall a. Confirm.Query a -> DSL Unit
confirmPeek _ = pure unit

rotaryPeek :: forall a. RotaryQuery a -> DSL Unit
rotaryPeek (Rotary.Selected opt _) = do
  modify $ _shareType .~ (Rotary.runOption opt).shareType
  modify $ _canGoFurther .~ false
  tokenGenSwitch
rotaryPeek _ = pure unit

groupPeek :: forall a. CascadeQuery a -> DSL Unit
groupPeek (Cascade.Selected _ _ _) = groupsUpdate
groupPeek (Cascade.Remove _ _) = groupsUpdate
groupPeek _ = pure unit
