module Model.File.Dialog.Mount where

import Data.Array (length)
import Data.Int (Int(), fromNumber)
import Data.Maybe (Maybe(..))
import Data.Foldable (all)
import Data.Tuple (Tuple(..), zip)
import Optic.Core ((.~), lens, LensP())
import Optic.Index (ix)

type MountDialogRec =
  { new :: Boolean
  , name :: String
  , hosts :: [MountHostRec]
  , user :: String
  , password :: String
  , props :: [MountPropRec]
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

hosts :: LensP MountDialogRec [MountHostRec]
hosts = lens _.hosts (_ { hosts = _ })

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
  , hosts: [initialMountHost]
  , user: ""
  , password: ""
  , props: [initialMountProp]
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
                   && all (\(Tuple h1 h2) -> h1.host == h2.host && h1.port == h2.port) (zip m1.hosts m2.hosts)
                   && m1.user == m2.user
                   && m1.password == m2.password
                   && length m1.props == length m2.props
                   && all (\(Tuple p1 p2) -> p1.name == p2.name && p1.value == p2.value) (zip m1.props m2.props)
