
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
import SlamData.Render.Icon as I

data Query a
  = Init a
  | FetchUsers a
  | SetFilter String a
  | SetGroupFilter QA.GroupPath a
  | DeleteUser QA.UserId a
  | EditUser QA.UserId a
  | Select Int a
  | HoverIn Int a
  | HoverOut a
  | HandleGroupFilter (AC.Message String) a

data GroupFilter
  = NoFilter
  | GroupFilter (Array QA.UserId)
  | InvalidGroupFilter String

data Message = RaiseDialog Dialog

type State =
  { filter ∷ String
  , users ∷ Maybe (Array QA.UserId)
  , allGroups ∷ Maybe (Array QA.GroupPath)
  , groupFilter ∷ GroupFilter
  , selected ∷ Maybe Int
  , hovered ∷ Maybe Int
  }

defaultState ∷ State
defaultState = { filter: "", users: Nothing, allGroups: Nothing, groupFilter: NoFilter, selected: Nothing, hovered: Nothing }

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
          [ HP.class_ (HH.ClassName "sd-admin-ui-users") ]
          [ HH.div
            [ HP.class_ (HH.ClassName "sd-admin-ui-users-header")]
            [ HH.div [ HP.class_ (H.ClassName "sd-admin-ui-users-search")]
                [ HH.div
                    [ HP.class_ (HH.ClassName "sd-admin-ui-users-search-icon") ]
                    [ I.searchSm ]
                , HH.input
                    [ HP.classes (HH.ClassName <$> ["sd-admin-ui-users-search-field", "form-control"])
                    , HP.type_ HP.InputText
                    , HP.placeholder "Search by user id"
                    , HE.onValueInput (HE.input \str → SetFilter str)
                    , HP.value state.filter
                    ]
                , HH.button
                    [ HP.class_ (HH.ClassName "sd-admin-ui-users-search-clear")
                    , HP.type_ HP.ButtonButton
                    , HE.onClick (HE.input_ (SetFilter ""))
                    , HP.enabled (state.filter /= "")
                    ]
                    [ R.clearFieldIcon "Clear search string" ]
                ]
            , HH.slot
                unit
                (AC.component AC.defaultConfig { containerClass = H.ClassName "sd-admin-ui-autocomplete"
                                               , placeholder = "Search by group"
                                               , autofirst = true
                                               , itemFilter = String.contains ∘ String.Pattern
                                               })
                (maybe [] (map QA.printGroupPath) state.allGroups)
                (HE.input HandleGroupFilter)
            , HH.button
                [ HE.onClick (HE.input_ FetchUsers)
                , HP.classes (map H.ClassName ["btn", "btn-success"])
                ]
                [ HH.text "New User" ]
            ]
          , HH.ul
              [ HP.class_ (HH.ClassName "sd-admin-ui-users-list") ]
              case state.users of
                -- TODO(Christoph): Show a spinner or something
                Nothing →
                  []
                Just users →
                  (Array.mapWithIndex renderUser (Array.filter (userFilter && groupFilter) users))
          ]
        where
          userFilter uid = String.contains (String.Pattern state.filter) (QA.runUserId uid)
          groupFilter userId = case state.groupFilter of
            NoFilter →
              true
            -- TODO(Christoph): Display a warning/error about the wrong filter.
            -- Maybe a red outline
            InvalidGroupFilter _ →
              true
            GroupFilter userIds →
              userId `Array.elem` userIds

          renderUser ix userId =
            let
              isActive = state.selected == Just ix || state.hovered == Just ix
            in
              HH.li
                [ HP.class_ (HH.ClassName "sd-admin-ui-user")
                , HE.onMouseEnter (HE.input_ (HoverIn ix))
                , HE.onMouseLeave (HE.input_ HoverOut)
                , HE.onClick (HE.input_ (Select ix))
                ]
                [ HH.div
                    [ HP.class_ (HH.ClassName "sd-admin-ui-user-label") ]
                    [ HH.text (QA.runUserId userId) ]
                , HH.div
                      [ HP.classes (HH.ClassName <$> ["sd-admin-ui-user-actions"] <> (guard (not isActive) $> "hidden")) ]
                    [ HH.a
                        [ HE.onClick (HE.input_ (DeleteUser userId)) ]
                        [ HH.text "Delete" ]
                    , HH.a
                        [ HE.onClick (HE.input_ (EditUser userId)) ]
                        [ HH.text "Permissions" ]
                    ]
                ]
      eval ∷ Query ~> DSL
      eval = case _ of
        Init next → do
          allGroups ← fetchAllGroups
          users ← allUsers
          H.modify (_ { allGroups = Just allGroups, users = Just users })
          pure next
        Select ix next → do
          H.modify (_ { selected = Just ix })
          pure next
        HoverIn ix next → do
          H.modify (_ { hovered = Just ix })
          pure next
        HoverOut next → do
          H.modify (_ { hovered = Nothing })
          pure next
        FetchUsers next → do
          users ← allUsers
          H.modify (_ { users = Just users })
          pure next
        SetFilter s next → do
          H.modify (_ { filter = s })
          pure next
        SetGroupFilter path next → do
          setGroupFilter path
          _ ← H.query unit (H.action (AC.Input (QA.printGroupPath path)))
          _ ← H.query unit (H.action (AC.Close AC.CuzSelect))
          pure next
        DeleteUser userId next → do
          H.raise (RaiseDialog (Dialog.ConfirmUserDeletion userId))
          pure next
        EditUser userId next → do
          groups ← crawlGroups userId
          H.raise (RaiseDialog (Dialog.EditUserPermissions { userId, groups }))
          pure next
        HandleGroupFilter msg next → case msg of
          AC.Changed "" → do
            H.modify (_ { groupFilter = NoFilter })
            pure next
          AC.Changed s → do
            case QA.parseGroupPath s of
              Right path → setGroupFilter path
              Left _ → H.modify (_ { groupFilter = InvalidGroupFilter s })
            pure next
          AC.Selected _ → pure next

setGroupFilter ∷ QA.GroupPath → DSL Unit
setGroupFilter path = do
  allGroups ← H.gets _.allGroups
  case allGroups of
    Just groups | path `Array.elem` groups → do
      users ← fetchTransitiveUsers path
      H.modify (_ { groupFilter = GroupFilter users })
    _ →
      H.modify (_ { groupFilter = InvalidGroupFilter (QA.printGroupPath path) })

allUsers ∷ ∀ m. Monad m ⇒ QuasarDSL m ⇒ m (Array QA.UserId)
allUsers = do
  groupInfo (QA.GroupPath Pathy.rootDir) >>= case _ of
    Left _ →
      -- TODO(Christoph): We should display an Error here
      pure []
    Right { allMembers } →
      pure allMembers

fetchAllGroups ∷ ∀ m . Monad m ⇒ QuasarDSL m ⇒ m (Array QA.GroupPath)
fetchAllGroups = do
  result ← groupInfo (QA.GroupPath Pathy.rootDir)
  pure (either (const []) _.subGroups result)

fetchTransitiveUsers
  ∷ ∀ m
  . Monad m
  ⇒ QuasarDSL m
  ⇒ QA.GroupPath
  → m (Array QA.UserId)
fetchTransitiveUsers path = do
  result ← groupInfo path
  pure (either (const []) _.allMembers result)

crawlGroups ∷ ∀ m . Monad m ⇒ QuasarDSL m ⇒ QA.UserId → m (Array QA.GroupPath)
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
