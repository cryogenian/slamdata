module Model.File.Dialog.Mount where

import Data.Array (length, zipWith)
import Data.Int (Int(), fromNumber)
import Data.Maybe (Maybe(..))
import Data.Foldable (and, all)
import Data.Tuple (Tuple(..), zip)
import Optic.Core ((.~), lens, LensP())

type MountDialogRec =
  { new :: Boolean
  , name :: String
  , connectionURI :: String
  , hosts :: [MountHostRec]
  , path :: String
  , user :: String
  , password :: String
  , props :: [MountPropRec]
  , message :: Maybe String
  , valid :: Boolean
  }

type MountHostRec =
  { host :: String
  , port :: String
  }

type MountPropRec =
  { name :: String
  , value :: String
  }

_name :: forall a. LensP { name :: String | a } String
_name = lens _.name (_ { name = _ })

_connectionURI :: LensP MountDialogRec String
_connectionURI = lens _.connectionURI (_ { connectionURI = _ })

_hosts :: LensP MountDialogRec [MountHostRec]
_hosts = lens _.hosts (_ { hosts = _ })

_path :: LensP MountDialogRec String
_path = lens _.path (_ { path = _ })

_user :: LensP MountDialogRec String
_user = lens _.user (_ { user = _ })

_password :: LensP MountDialogRec String
_password = lens _.password (_ { password = _ })

_props :: LensP MountDialogRec [MountPropRec]
_props = lens _.props (_ { props = _ })

_host :: LensP MountHostRec String
_host = lens _.host (_ { host = _ })

_port :: LensP MountHostRec String
_port = lens _.port (_ { port = _ })

_value :: LensP MountPropRec String
_value = lens _.value (_ { value = _ })

initialMountDialog :: MountDialogRec
initialMountDialog =
  { new: true
  , name: ""
  , connectionURI: ""
  , hosts: [initialMountHost, initialMountHost]
  , path: ""
  , user: ""
  , password: ""
  , props: [initialMountProp]
  , message: Nothing
  , valid: false
  }

initialMountHost :: MountHostRec
initialMountHost =
  { host: ""
  , port: ""
  }

initialMountProp :: MountPropRec
initialMountProp =
  { name: ""
  , value: ""
  }

isEmptyHost :: MountHostRec -> Boolean
isEmptyHost h = h.host == "" && h.port == ""

isEmptyProp :: MountPropRec -> Boolean
isEmptyProp p = p.name == "" && p.value == ""
