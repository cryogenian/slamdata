module SlamData.FileSystem.Dialog.Permissions.Component.State where

import SlamData.Prelude

import Control.Monad.Eff.Ref (Ref)

import Data.Lens (LensP, lens)

import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Auth.Permission as Qp

data ShareType = Code | User | Group

instance eqShareType :: Eq ShareType where
  eq Code Code = true
  eq User User = true
  eq Group Group = true
  eq _ _ = false

type State =
  {
    shareType :: ShareType
  , permissions :: Maybe Qp.Permissions
  , resource :: R.Resource
  , confirm :: Maybe Qp.PermissionShareRequest
  , error :: Maybe String
  , sending :: Boolean
  , zRef :: Maybe (Ref (Maybe String))
  , canGoFurther :: Boolean
  , groups :: Maybe (Array Qp.Group)
  }

_shareType :: forall a r. LensP {shareType :: a|r} a
_shareType = lens _.shareType _{shareType = _}

_permissions :: forall a r. LensP {permissions :: a|r} a
_permissions = lens _.permissions _{permissions = _}

_resource :: forall a r. LensP {resource :: a|r} a
_resource = lens _.resource _{resource = _}

_confirm :: forall a r. LensP {confirm :: a|r} a
_confirm = lens _.confirm _{confirm = _}

_error :: forall a r. LensP {error :: a|r} a
_error = lens _.error _{error = _}

_sending :: forall a r. LensP {sending :: a|r} a
_sending = lens _.sending _{sending = _}

_zRef :: forall a r. LensP {zRef :: a|r} a
_zRef = lens _.zRef _{zRef = _}

_canGoFurther :: forall a r. LensP {canGoFurther :: a|r} a
_canGoFurther = lens _.canGoFurther _{canGoFurther = _}

_groups :: forall a r. LensP {groups :: a|r} a
_groups = lens _.groups _{groups = _}

initialState :: R.Resource -> State
initialState res =
  {
    shareType: User
  , permissions: Nothing
  , resource: res
  , confirm: Nothing
  , error: Nothing
  , sending: false
  , zRef: Nothing
  , canGoFurther: false
  , groups: Nothing
  }
