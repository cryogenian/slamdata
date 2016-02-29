{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Dialog.Share.Confirm.Component where

import Prelude

import Control.MonadPlus (guard)

import Data.Array as Arr
import Data.Either as E
import Data.Functor (($>))
import Data.Path.Pathy as Pt
import Data.NonEmpty as Ne
import Data.Foldable as F

import Halogen
import Halogen.HTML.Indexed as H

import Quasar.Auth.Permission as P

import SlamData.Effects (Slam())
import SlamData.FileSystem.Resource as R

data Query a
  = Set P.PermissionShareRequest a

type PermissionShareConfirmDSL
  = ComponentDSL P.PermissionShareRequest Query Slam


comp :: Component P.PermissionShareRequest Query Slam
comp = component render eval

render :: P.PermissionShareRequest -> ComponentHTML Query
render state =
  if P.isPermissionsEmpty state.permissions
  then
    H.div_ [ H.p_ [ H.text "There is no set permissions to share" ] ]
  else
    H.div_
      [
        H.p_ [ H.text "You are going to provide permissions to:" ]
      , H.ul_ $ renderPermissions state.permissions
      , H.p_ $ [ H.text "for: " ] <> renderResource state.resource
      , H.p_ [ H.text
                 $ "to following "
                 <> (if P.requestForUsers state
                     then "users"
                     else "groups")
                 <> ":"
             ]
      , H.ul_ $ renderTargets state.targets
      ]
  where
  renderPermissions :: P.Permissions -> Array (ComponentHTML Query)
  renderPermissions p =
    map (H.li_ <<< pure <<< H.text)
    $ Arr.concat
      [
        (guard p.add) $> "ADD"
      , (guard p.read) $> "READ"
      , (guard p.modify) $> "MODIFY"
      , (guard p.delete) $> "DELETE"
      ]
  renderResource :: R.Resource -> Array (ComponentHTML Query)
  renderResource r =
    let
      resourceType (R.File _) = "File"
      resourceType (R.Notebook _) = "Notebook"
      resourceType (R.Directory _) = "Directory"
      resourceType (R.Mount (R.View _)) = "View mount"
      resourceType (R.Mount (R.Database _)) = "Database"
      msg = (resourceType r) <> ": " <> (R.resourcePath r)
    in
      [ H.strong_ [ H.text msg ] ]

  renderTargets
    :: E.Either (Ne.NonEmpty Array P.Group) (Ne.NonEmpty Array P.User)
    -> Array (ComponentHTML Query)
  renderTargets targets =
    F.foldMap (pure <<< H.li_ <<< pure <<< H.text)
    $ E.either (map (P.runGroup >>> Pt.printPath)) (map P.runUser) targets

eval :: Natural Query PermissionShareConfirmDSL
eval (Set p next) = set p $> next
