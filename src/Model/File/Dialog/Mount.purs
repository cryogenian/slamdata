module Model.File.Dialog.Mount where

import Data.Array (length, zipWith)
import Data.Int (Int(), fromNumber)
import Data.Maybe (Maybe(..))
import Data.Foldable (and, all)
import Data.Tuple (Tuple(..), zip)
import Optic.Core ((.~), lens, LensP())
import Optic.Index (ix)

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

name :: forall a. LensP { name :: String | a } String
name = lens _.name (_ { name = _ })

connectionURI :: LensP MountDialogRec String
connectionURI = lens _.connectionURI (_ { connectionURI = _ })

hosts :: LensP MountDialogRec [MountHostRec]
hosts = lens _.hosts (_ { hosts = _ })

path :: LensP MountDialogRec String
path = lens _.path (_ { path = _ })

user :: LensP MountDialogRec String
user = lens _.user (_ { user = _ })

password :: LensP MountDialogRec String
password = lens _.password (_ { password = _ })

props :: LensP MountDialogRec [MountPropRec]
props = lens _.props (_ { props = _ })

host :: LensP MountHostRec String
host = lens _.host (_ { host = _ })

port :: LensP MountHostRec String
port = lens _.port (_ { port = _ })

value :: LensP MountPropRec String
value = lens _.value (_ { value = _ })

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
