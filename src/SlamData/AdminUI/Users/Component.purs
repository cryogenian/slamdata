
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

module SlamData.AdminUI.Users.Component where


import SlamData.Prelude

import Data.Array as Array
import Data.List as L
import Data.List as List
import Data.Path.Pathy as Pathy
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Dialog.Component (Dialog)
import SlamData.AdminUI.Dialog.Component as Dialog
import SlamData.Autocomplete.Component as AC
import SlamData.Monad (Slam)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Security (groupInfo, removeUsersFromGroup)
import SlamData.Render.Common as R

data Query a
  = Init a
  | FetchUsers a
  | SetFilter String a
  | DeleteUser QA.UserId a
  | EditUser QA.UserId a

data Message = RaiseDialog Dialog

type State =
  { filter ∷ String
  , users ∷ L.List QA.UserId
  }

defaultState ∷ State
defaultState = { filter: "", users: L.Nil }

type ChildSlot = Unit

type HTML = H.ParentHTML Query (AC.Query String) ChildSlot Slam
type DSL = H.ParentDSL State Query (AC.Query String) ChildSlot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.lifecycleParentComponent
    { render
    , eval
    , receiver: const Nothing
    , initialState: const defaultState
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
    where
      render state =
        HH.div
          [ HP.class_ (HH.ClassName "users") ]
          [ HH.fieldset_
            [ HH.label_
              [ HH.text "Search"
              , HH.input
                [ HP.class_ (HH.ClassName "form-control")
                , HP.type_ HP.InputText
                , HP.placeholder "Search string"
                , HE.onValueInput (HE.input \str → SetFilter str)
                , HP.value state.filter
                ]
              , HH.button
                  [ HE.onClick (HE.input_ (SetFilter ""))]
                [ R.clearFieldIcon "Clear search string" ]
              , HH.button
                  [ HE.onClick (HE.input_ FetchUsers)]
                [ R.busyFieldIcon "Clear search string" ]
              , HH.slot unit (AC.component AC.defaultConfig) ["hello"] (const Nothing)
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
                      [ HE.onClick (HE.input_ (DeleteUser uid))
                      ]
                      [ HH.text "Delete" ]
                  , HH.button
                      [ HE.onClick (HE.input_ (EditUser uid))
                      ]
                      [ HH.text "Permissions" ]
                  ]
              ]
      eval ∷ Query ~> DSL
      eval = case _ of
        Init next → pure next
        FetchUsers next →
          groupInfo (QA.GroupPath Pathy.rootDir) >>= case _ of
            Left _ →
              -- TODO(Christoph): We should display an Error here
              pure next
            Right { allMembers } → do
              H.modify (_ { users = List.fromFoldable allMembers })
              pure next
        SetFilter s next → do
          H.modify (_ { filter = s })
          pure next
        DeleteUser userId next → do
          H.raise (RaiseDialog (Dialog.ConfirmUserDeletion userId))
          pure next
        EditUser userId next → do
          groups ← crawlGroups userId
          H.raise (RaiseDialog (Dialog.EditUserPermissions { userId, groups }))
          pure next

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
