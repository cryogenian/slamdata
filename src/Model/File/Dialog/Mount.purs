module Model.File.Dialog.Mount where

import Prelude
import Data.Array (length, zipWith, replicate, zip)
import Data.Char (fromCharCode)
import Data.Either (either)
import Data.Foldable (and, all)
import Data.List (fromList)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Path.Pathy (rootDir, printPath)
import Data.StrMap (toList)
import Data.Tuple (Tuple(..))
import Data.URI (printAbsoluteURI)
import Data.URI.Types
import Model.Path
import Optic.Core

import qualified Data.String as S

type MountDialogRec =
  { new :: Boolean
  , parent :: DirPath
  , name :: String
  , connectionURI :: String
  , hosts :: (Array MountHostRec)
  , path :: String
  , user :: String
  , password :: String
  , props :: (Array MountPropRec)
  , message :: Maybe String
  , externalValidationError :: Maybe String
  , valid :: Boolean
  , inProgress :: Boolean
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

_hosts :: LensP MountDialogRec (Array MountHostRec)
_hosts = lens _.hosts (_ { hosts = _ })

_path :: LensP MountDialogRec String
_path = lens _.path (_ { path = _ })

_user :: LensP MountDialogRec String
_user = lens _.user (_ { user = _ })

_password :: LensP MountDialogRec String
_password = lens _.password (_ { password = _ })

_inProgress :: LensP MountDialogRec Boolean
_inProgress = lens _.inProgress (_ { inProgress = _ })

_message :: LensP MountDialogRec (Maybe String)
_message = lens _.message (_ { message = _ })

_externalValidationError :: LensP MountDialogRec (Maybe String)
_externalValidationError = lens _.externalValidationError (_ { externalValidationError = _ })

_props :: LensP MountDialogRec (Array MountPropRec)
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
  , externalValidationError: Nothing
  , valid: false
  , inProgress: false
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

hostsFromURI :: AbsoluteURI -> Array MountHostRec
hostsFromURI (AbsoluteURI _ (HierarchicalPart (Just (Authority _ hs)) _) _) = go <$> hs
  where
  go :: Tuple Host (Maybe Port) -> MountHostRec
  go (Tuple h p) = { host: getHost h, port: maybe "" show p }
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
  maybe ui (\ix -> S.take ix ui) $ S.indexOf ":" ui
userFromURI _ = ""

passwordFromURI :: AbsoluteURI -> String
passwordFromURI (AbsoluteURI _ (HierarchicalPart (Just (Authority (Just ui) _)) _) _) =
  maybe "" (\ix -> S.drop (ix + 1) ui) $ S.indexOf ":" ui
passwordFromURI _ = ""

setURIPassword :: String -> AbsoluteURI -> AbsoluteURI
setURIPassword pass (AbsoluteURI s (HierarchicalPart (Just (Authority (Just ui) hs)) p) q) =
  let mbIx = S.indexOf ":" ui
      user = maybe ui (\ix -> S.take ix ui) mbIx
      pass' = if pass == "" then "" else ":" ++ pass
  in AbsoluteURI s (HierarchicalPart (Just (Authority (Just $ user ++ pass') hs)) p) q
setURIPassword _ uri = uri

propsFromURI :: AbsoluteURI -> Array MountPropRec
propsFromURI (AbsoluteURI _ _ (Just (Query qs))) =
  fromList <<< (go <$>) <<< toList $ qs
  where
  go :: Tuple String (Maybe String) -> MountPropRec
  go (Tuple k v) = { name: k, value: fromMaybe "" v }
propsFromURI _ = []

hidePassword :: String -> String
hidePassword s = S.joinWith "" $ replicate (S.length s) (S.fromChar $ fromCharCode 8226)
