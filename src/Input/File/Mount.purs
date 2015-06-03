module Input.File.Mount
  ( MountInput(..)
  , inputMount
  ) where

import Data.Array (filter, replicate, null)
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust, isNothing)
import Data.URI (runParseAbsoluteURI, printAbsoluteURI)
import Model.File.Dialog (Dialog(MountDialog))
import Model.File.Dialog.Mount
import qualified Data.String as Str
import qualified Data.String.Regex as Rx
import Utils.ConnectionURI (toURI)

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
      validation = validate d'.name hosts
  in MountDialog d' { connectionURI = connectionURI
                    , hosts = if null hosts
                              then [initialMountHost, initialMountHost]
                              else hosts ++ [initialMountHost]
                    , props = props ++ [initialMountProp]
                    , message = validation
                    , valid = isNothing validation
                    }
inputMount (MountDialog d) (UpdateConnectionURI "") =
  let parent = d.parent
  in MountDialog initialMountDialog { parent = parent }
inputMount (MountDialog d) (UpdateConnectionURI uri) =
  MountDialog $ case runParseAbsoluteURI uri of
    Left _ -> d { connectionURI = uri
                , message = Just "Pasted value does not appear to be a valid connection URI"
                , valid = false
                }
    Right uri' ->
      if schemeFromURI uri' /= "mongodb"
      then d { connectionURI = uri
             , message = Just "Pasted value does not appear to be a mongodb connection URI"
             , valid = false
             }
      else
        let hosts = hostsFromURI uri'
            validation = validate d.name hosts
        in d { connectionURI = printAbsoluteURI uri'
             , hosts = hosts ++ [initialMountHost]
             , path = pathFromURI uri'
             , user = userFromURI uri'
             , password = passwordFromURI uri'
             , props = propsFromURI uri' ++ [initialMountProp]
             , message = validation
             , valid = isNothing validation
             }
inputMount (MountDialog d) ClearMessage = MountDialog $ d { message = Nothing }
inputMount dialog _ = dialog

mkURI :: String -> String -> String -> Array MountHostRec -> Array MountPropRec -> String
mkURI path user password hosts props =
  if any isValidHost hosts
  then toURI { path: nonEmpty path
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

validate :: String -> [MountHostRec] -> Maybe String
validate "" _ = Just "Please enter a name for the mount"
validate _ [] = Just "Please enter at least one host"
validate _ _  = Nothing
