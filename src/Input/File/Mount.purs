{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Input.File.Mount
  ( MountInput(..)
  , inputMount
  ) where

import Prelude
import Data.Array (filter, replicate, null)
import Data.Char (fromCharCode)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
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

inputMount :: MountInput -> Dialog -> Dialog
inputMount (ValueChanged fn) (MountDialog d) =
  let d' = fn d
      hosts = (filter (not <<< isEmptyHost) d'.hosts)
      props = (filter (not <<< isEmptyProp) d'.props)
      connectionURI = mkURI d'.path d'.user d'.password hosts props
      validation = validate d' hosts
  in MountDialog d' { connectionURI = connectionURI
                    , hosts = if null hosts
                              then [initialMountHost, initialMountHost]
                              else hosts ++ [initialMountHost]
                    , props = props ++ [initialMountProp]
                    , message = validation
                    , valid = isNothing validation
                    }
inputMount (UpdateConnectionURI "") (MountDialog d) =
  let parent = d.parent
  in MountDialog initialMountDialog { parent = parent }
inputMount (UpdateConnectionURI uri) (MountDialog d) =
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
            validation = validate d hosts
        in d { connectionURI = printAbsoluteURI uri'
             , hosts = hosts ++ [initialMountHost]
             , path = pathFromURI uri'
             , user = userFromURI uri'
             , password = passwordFromURI uri'
             , props = propsFromURI uri' ++ [initialMountProp]
             , message = validation
             , valid = isNothing validation
             }
inputMount ClearMessage (MountDialog d) = MountDialog $ d { message = Nothing, externalValidationError = Nothing }
inputMount _ dialog = dialog

mkURI :: String -> String -> String -> Array MountHostRec -> Array MountPropRec -> String
mkURI path user password hosts props =
  if any isValidHost hosts
  then toURI { path: nonEmpty path
             , credentials: { user: _, password: _ }
                            <$> nonEmpty user
                            <*> nonEmpty password
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

validate :: MountDialogRec -> Array MountHostRec -> Maybe String
validate d hosts =
  either Just (const $ Nothing) do
    case Tuple d.new d.name of
      Tuple true "" -> Left "Please enter a name for the mount"
      _ -> Right unit
    case hosts of
      [] -> Left "Please enter at least one host"
      _ -> Right unit
