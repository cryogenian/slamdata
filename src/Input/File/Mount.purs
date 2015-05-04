module Input.File.Mount
  ( MountInput(..)
  , inputMount
  ) where

import Data.Array (filter, replicate)
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Foldable (any)
import Model.File.Dialog (Dialog(MountDialog))
import Model.File.Dialog.Mount (MountDialogRec(), MountHostRec(), MountPropRec(), initialMountDialog, initialMountHost, initialMountProp)
import Utils.ConnectionURI (parse, toURI)
import qualified Data.String as Str
import qualified Data.String.Regex as Rx

data MountInput
  = ValueChanged (MountDialogRec -> MountDialogRec)
  | UpdateConnectionURI String
  | ClearMessage

inputMount :: Dialog -> MountInput -> Dialog
inputMount (MountDialog d) (ValueChanged fn) =
  let d' = fn d
      hosts = (filter (not <<< isEmptyHost) d'.hosts)
      props = (filter (not <<< isEmptyProp) d'.props)
      connectionURI = mkURI d'.path d'.user d'.password hosts props
  in MountDialog d' { connectionURI = connectionURI
                    , hosts = hosts ++ [initialMountHost]
                    , props = props ++ [initialMountProp]
                    , message = Nothing
                    , valid = true
                    }
inputMount (MountDialog d) (UpdateConnectionURI "") = MountDialog initialMountDialog
inputMount (MountDialog d) (UpdateConnectionURI uri) =
  MountDialog $ case parse uri of
    Left _ -> d { connectionURI = uri
                , message = Just "Pasted value does not appear to be a valid connection URI"
                , valid = false
                }
    Right params ->
      d { connectionURI = uri
        , path = fromMaybe "" params.name
        , user = maybe "" (_.user) params.credentials
        , password = maybe "" (_.password) params.credentials
        , hosts = ((\host -> host { port = fromMaybe "" host.port }) <$> params.hosts) ++ [initialMountHost]
        , props = params.props ++ [initialMountProp]
        , message = Nothing
        , valid = true
        }
inputMount (MountDialog d) ClearMessage = MountDialog $ d { message = Nothing }
inputMount dialog _ = dialog

isEmptyHost :: MountHostRec -> Boolean
isEmptyHost h = h.host == "" && h.port == ""

isEmptyProp :: MountPropRec -> Boolean
isEmptyProp p = p.name == "" && p.value == ""

mkURI :: String -> String -> String -> Array MountHostRec -> Array MountPropRec -> String
mkURI name user password hosts props =
  if any isValidHost hosts
  then toURI { name: nonEmpty name
             , credentials: { user: _, password: _ }
                            <$> nonEmpty user
                            <*> nonEmpty (hidePassword password)
             , hosts: (\h -> h { port = nonEmpty h.port }) <$> hosts
             , props: props
             }
  else ""
  where
  isValidHost :: MountHostRec -> Boolean
  isValidHost { host: host } = isJust $ nonEmpty host

nonEmpty :: String -> Maybe String
nonEmpty s | Rx.test rxEmpty s = Nothing
nonEmpty s = Just s

rxEmpty :: Rx.Regex
rxEmpty = Rx.regex "^\\s*$" Rx.noFlags

hidePassword :: String -> String
hidePassword s = Str.joinWith "" $ replicate (Str.length s) (Str.fromChar $ fromCharCode 8226)
