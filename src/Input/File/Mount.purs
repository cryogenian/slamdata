module Input.File.Mount
  ( MountInput(..)
  , inputMount
  ) where

import Data.Array (filter)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (Either(..))
import Model.File.Dialog (Dialog(MountDialog))
import Model.File.Dialog.Mount (MountDialogRec(), MountHostRec(), MountPropRec(), initialMountHost, initialMountProp)
import Utils.ConnectionURI (parse)
import qualified Data.String as Str

data MountInput
  = ValueChanged (MountDialogRec -> MountDialogRec)
  | UpdateConnectionURI String

inputMount :: Dialog -> MountInput -> Dialog
inputMount (MountDialog d) (ValueChanged fn) =
  let d' = fn d
  in MountDialog d' { hosts = (filter (not <<< isEmptyHost) d'.hosts) ++ [initialMountHost]
                    , props = (filter (not <<< isEmptyProp) d'.props) ++ [initialMountProp]
                    , message = Nothing
                    }
inputMount (MountDialog d) (UpdateConnectionURI uri) =
  MountDialog $ case parse uri of
    Left _ -> d { connectionURI = uri
                , message = Just "Pasted value does not appear to be a valid MongoDB connection URI"
                }
    Right params ->
      d { connectionURI = uri
        , name = fromMaybe "" params.name
        , user = maybe "" (_.user) params.credentials
        , password = maybe "" (_.password) params.credentials
        , hosts = ((\host -> host { port = fromMaybe "" host.port }) <$> params.hosts) ++ [initialMountHost]
        , props = params.props ++ [initialMountProp]
        }
inputMount dialog _ = dialog

isEmptyHost :: MountHostRec -> Boolean
isEmptyHost h = h.host == "" && h.port == ""

isEmptyProp :: MountPropRec -> Boolean
isEmptyProp p = p.name == "" && p.value == ""
