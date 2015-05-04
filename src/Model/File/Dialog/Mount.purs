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
  , hosts: [initialMountHost]
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

eqMountDialog :: MountDialogRec -> MountDialogRec -> Boolean
eqMountDialog m1 m2 = m1.name == m2.name
                   && length m1.hosts == length m2.hosts
                   && and (m1.hosts `zipWith eqMountHost` m2.hosts)
                   && m1.path == m2.path
                   && m1.user == m2.user
                   && m1.password == m2.password
                   && length m1.props == length m2.props
                   && and (m1.props `zipWith eqMountProp` m2.props)

eqMountHost :: MountHostRec -> MountHostRec -> Boolean
eqMountHost m1 m2 = m1.host == m2.host
                 && m1.port == m2.port

eqMountProp :: MountPropRec -> MountPropRec -> Boolean
eqMountProp m1 m2 = m1.name == m2.name
                 && m1.value == m2.value
