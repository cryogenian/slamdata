{-
Copyright 2017 SlamData, Inc.

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

module SlamData.AdminUI.Users where

import SlamData.Prelude

import Data.Array as Array
import Data.List as List
import Data.Path.Pathy as Pathy
import Data.String as String
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Types as AT
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Security (groupInfo, removeUsersFromGroup)
import SlamData.Render.Common as R

type User =
  { id ∷ QA.UserId
  }

renderForm ∷ AT.UsersState → Array AT.HTML
renderForm (AT.UsersState state) =
  [ HH.fieldset_
      [ HH.label_
          [ HH.text "Search"
          , HH.input
              [ HP.class_ (HH.ClassName "form-control")
              , HP.type_ HP.InputText
              , HP.placeholder "Search string"
              , HE.onValueInput (HE.input \str → AT.SetUsers (AT.UsersState (state {filter = str})))
              , HP.value state.filter
              ]
          , HH.button
              [ HE.onClick (HE.input_ (AT.SetUsers (AT.UsersState (state {filter = ""}))))
              ]
              [ R.clearFieldIcon "Clear search string" ]
          , HH.button
              [ HE.onClick (HE.input_ (AT.UQ ∘ AT.FetchUsers))
              ]
              [ R.busyFieldIcon "Clear search string" ]
          ]
      ]
      , HH.ul
          [ HP.class_ (HH.ClassName "sd-admin-ui-users-list") ]
          (Array.fromFoldable (map renderUser (List.filter userFilter state.users)))
  ]
  where
    userFilter uid = String.contains (String.Pattern state.filter) (QA.runUserId uid)

    renderUser uid =
      HH.li
        [ HP.class_ (HH.ClassName "sd-admin-ui-user") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "sd-admin-ui-user-label") ]
            [ HH.text (QA.runUserId uid) ]
        , HH.div
            [ HP.class_ (HH.ClassName "sd-admin-ui-user-actions") ]
            [ HH.button
                [ HE.onClick (HE.input_ (AT.UQ ∘ AT.DeleteUser uid))
                ]
                [ HH.text "Delete" ]
            , HH.button
                [ HE.onClick (HE.input_ (AT.UQ ∘ AT.EditUser uid))
                ]
                [ HH.text "Permissions" ]
            ]
        ]

allUsers
  ∷ ∀ m
  . Monad m
  ⇒ QuasarDSL m
  ⇒ m (Array QA.UserId)
allUsers = do
  groupInfo (QA.GroupPath Pathy.rootDir) >>= case _ of
    Left _ →
      -- TODO(Christoph): We should display an Error here
      pure []
    Right { allMembers } →
      pure allMembers

crawlGroups
  ∷ ∀ m
  . Monad m
  ⇒ QuasarDSL m
  ⇒ QA.UserId
  → m (Array QA.GroupPath)
crawlGroups userId =
  map
    -- Drop the root directory, because every User is member of it implicitly
    (fromMaybe [] ∘ Array.tail)
    (go (QA.GroupPath Pathy.rootDir))
  where
    go path =
      groupInfo path >>= case _ of
        Left _ →
          -- TODO(Christoph): We should display an Error here, but I'm not sure
          -- if we should still continue
          pure []
        Right { allMembers, subGroups, members } | userId `Array.elem` allMembers → do
          -- We only consider the group hierarchy one level at a time, because we
          -- need to filter on group membership
          paths ← traverse go (Array.filter (isDirectSubgroup path) subGroups)
          let addPath = if userId `Array.elem` members then Array.cons path else id
          pure (addPath (fold paths))
        Right _ →
          -- The user did not exist within this part of the group hierarchy
          pure []

isDirectSubgroup ∷ QA.GroupPath → QA.GroupPath → Boolean
isDirectSubgroup (QA.GroupPath parent) (QA.GroupPath child) =
  case Pathy.peel child of
    Nothing → -- Should never happen
      false
    Just (childPrefix × _) →
      Pathy.identicalPath parent childPrefix

deleteUser ∷ ∀ m. Monad m ⇒ QuasarDSL m ⇒ QA.UserId → m Unit
deleteUser userId = do
  groups ← crawlGroups userId
  for_ groups \group →
    removeUsersFromGroup group [userId]
