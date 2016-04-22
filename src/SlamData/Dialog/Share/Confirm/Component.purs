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

module SlamData.Dialog.Share.Confirm.Component
  ( Query(..)
  , State
  , comp
  ) where

import SlamData.Prelude

import Data.Array as Arr
import Data.Path.Pathy as Pt
import Data.NonEmpty as Ne

import Halogen as H
import Halogen.HTML.Indexed as HH

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Auth.Permission as P

type State
  = P.PermissionShareRequest

data Query a
  = Set State a

type HTML
  = H.ComponentHTML Query

type PermissionShareConfirmDSL
  = H.ComponentDSL State Query Slam

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> HTML
render state =
  if P.isPermissionsEmpty state.permissions
  then
    HH.div_ [ HH.p_ [ HH.text "There is no set permissions to share" ] ]
  else
    HH.div_
      [ HH.p_ [ HH.text "You are going to provide permissions to:" ]
      , HH.ul_ $ renderPermissions state.permissions
      , HH.p_ $ [ HH.text "for: " ] <> renderResource state.resource
      , HH.p_
          [ HH.text
              $ "to following "
              <> (if P.requestForUsers state then "users" else "groups")
              <> ":"
          ]
      , HH.ul_ $ renderTargets state.targets
      ]
  where
  renderPermissions :: P.Permissions -> Array HTML
  renderPermissions p =
    map (HH.li_ <<< pure <<< HH.text)
    $ Arr.concat
      [ (guard p.add) $> "ADD"
      , (guard p.read) $> "READ"
      , (guard p.modify) $> "MODIFY"
      , (guard p.delete) $> "DELETE"
      ]
  renderResource :: R.Resource -> Array HTML
  renderResource r =
    let
      resourceType (R.File _) = "File"
      resourceType (R.Notebook _) = "Notebook"
      resourceType (R.Directory _) = "Directory"
      resourceType (R.Mount (R.View _)) = "View mount"
      resourceType (R.Mount (R.Database _)) = "Database"
      msg = (resourceType r) <> ": " <> (R.resourcePath r)
    in
      [ HH.strong_ [ HH.text msg ] ]

  renderTargets
    :: Either (Ne.NonEmpty Array P.Group) (Ne.NonEmpty Array P.User)
    -> Array HTML
  renderTargets targets =
    foldMap (pure <<< HH.li_ <<< pure <<< HH.text)
    $ either (map (P.runGroup >>> Pt.printPath)) (map P.runUser) targets

eval :: Natural Query PermissionShareConfirmDSL
eval (Set p next) = H.set p $> next
