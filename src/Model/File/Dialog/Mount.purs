module Model.File.Dialog.Mount where

import Data.Array (length, zipWith)
import Data.Either (either)
import Data.Foldable (and, all)
import Data.Int (Int(), fromNumber, toNumber)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Path.Pathy (rootDir, printPath)
import Data.String (indexOf, take, drop)
import Data.StrMap (toList)
import Data.Tuple (Tuple(..), zip)
import Data.URI (printAbsoluteURI)
import Data.URI.Types
import Model.Path
import Optic.Core ((.~), lens, LensP())

type MountDialogRec =
  { new :: Boolean
  , parent :: DirPath
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
  , parent: rootDir
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

mountDialogFromURI :: AbsoluteURI -> MountDialogRec
mountDialogFromURI uri =
  initialMountDialog { connectionURI = printAbsoluteURI uri
                     , hosts = hostsFromURI uri ++ [initialMountHost]
                     , path = pathFromURI uri
                     , user = userFromURI uri
                     , password = passwordFromURI uri
                     , props = propsFromURI uri ++ [initialMountProp]
                     }

schemeFromURI :: AbsoluteURI -> String
schemeFromURI (AbsoluteURI (Just (URIScheme s)) _ _) = s
schemeFromURI _ = ""

hostsFromURI :: AbsoluteURI -> [MountHostRec]
hostsFromURI (AbsoluteURI _ (HierarchicalPart (Just (Authority _ hs)) _) _) = go <$> hs
  where
  go :: Tuple Host (Maybe Port) -> MountHostRec
  go (Tuple h p) = { host: getHost h, port: maybe "" (show <<< toNumber) p }
  getHost :: Host -> String
  getHost (IPv6Address s) = s
  getHost (IPv4Address s) = s
  getHost (NameAddress s) = s
hostsFromURI _ = []

pathFromURI :: AbsoluteURI -> String
pathFromURI (AbsoluteURI _ (HierarchicalPart _ (Just p)) _) = either printPath printPath p
pathFromURI _ = ""

userFromURI :: AbsoluteURI -> String
userFromURI (AbsoluteURI _ (HierarchicalPart (Just (Authority (Just ui) _)) _) _) =
  let ix = indexOf ":" ui
  in if ix == -1
     then ui
     else take ix ui
userFromURI _ = ""

passwordFromURI :: AbsoluteURI -> String
passwordFromURI (AbsoluteURI _ (HierarchicalPart (Just (Authority (Just ui) _)) _) _) =
  let ix = indexOf ":" ui
  in if ix == -1
     then ""
     else drop (ix + 1) ui
passwordFromURI _ = ""

propsFromURI :: AbsoluteURI -> [MountPropRec]
propsFromURI (AbsoluteURI _ _ (Just (Query qs))) = go <$> toList qs
  where
  go :: Tuple String (Maybe String) -> MountPropRec
  go (Tuple k v) = { name: k, value: fromMaybe "" v }
propsFromURI _ = []
