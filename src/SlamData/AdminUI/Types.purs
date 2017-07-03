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

module SlamData.AdminUI.Types where

import SlamData.Prelude

import Data.List (List)
import Data.List as L
import Halogen as H
import Halogen.Component.ChildPath as CP
import Quasar.Advanced.Types as QA
import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Component as Miller
import Utils.DOM as DOM

type MillerQuery = Miller.Query GroupItem QA.GroupPath GroupMessage

type ChildQuery = MillerQuery ⨁ Const Void
type ChildSlot = Unit ⊹ Void

type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam
type DSL = H.ParentDSL State Query ChildQuery ChildSlot Void Slam

data Query a
  = Init a
  | Open a
  | Close a
  | SetActive TabIndex a
  | SetMySettings MySettingsState a
  | SetDatabase DatabaseState a
  | SetServer ServerState a
  | SetUsers UsersState a
  | SetGroups GroupsState a
  | DefaultThemeChanged String a
  | HandleColumns (Miller.Message GroupItem QA.GroupPath) a
  | HandleColumnOrItem GroupMessage a

data TabIndex
  = MySettings
  | Database
  | Server
  | Authentication
  | Users
  | Groups

derive instance eqTabIndex ∷ Eq TabIndex
derive instance ordTabIndex ∷ Ord TabIndex
instance showTabIndex ∷ Show TabIndex where
  show = tabTitle

allTabs ∷ List TabIndex
allTabs =
  -- L.fromFoldable [MySettings, Database, Server, Authentication, Users, Groups]
  L.fromFoldable [Groups]

tabTitle ∷ TabIndex → String
tabTitle = case _ of
  MySettings → "My Settings"
  Database → "Database"
  Server → "Server"
  Authentication → "Authentication"
  Users → "Users"
  Groups → "Groups"

type State =
  { open ∷ Boolean
  , active ∷ TabIndex
  , formState ∷
      { mySettings ∷ MySettingsState
      , database ∷ DatabaseState
      , server ∷ ServerState
      , users ∷ UsersState
      , groups ∷ GroupsState
      }
  }

newtype MySettingsState = MySettingsState
  { homeDirectory ∷ String
  , isolateArtifacts ∷ Boolean
  , isolateArtifactsDirectory ∷ String
  , defaultTheme ∷ String
  }

derive instance newtypeMySettingsState ∷ Newtype MySettingsState _

defaultMySettingsState ∷ MySettingsState
defaultMySettingsState =
  MySettingsState
    { homeDirectory: ""
    , isolateArtifacts: false
    , isolateArtifactsDirectory: ""
    , defaultTheme: "Dark"
    }

type PostgresCon =
  { server ∷ String
  , port ∷ Int
  , username ∷ String
  , password ∷ String
  , database ∷ String
  , custom ∷ Tuple String String
  }

defaultPostgresCon ∷ PostgresCon
defaultPostgresCon =
  { server: "localhost"
  , port: 5432
  , username: ""
  , password: ""
  , database: ""
  , custom: Tuple "" ""
  }

newtype DatabaseState = DatabaseState
  { isExternal ∷ Boolean
  , databaseFile ∷ String
  , postgresCon ∷ PostgresCon
  }

defaultDatabaseState ∷ DatabaseState
defaultDatabaseState =
  DatabaseState
    { isExternal: false
    , databaseFile: ""
    , postgresCon: defaultPostgresCon
    }

newtype ServerState = ServerState
  { port ∷ Int
  , logFileLocation ∷ String
  , enableCustomSSL ∷ Boolean
  }

defaultServerState ∷ ServerState
defaultServerState = ServerState { port: 27012, logFileLocation: "", enableCustomSSL: false }

newtype UsersState = UsersState
  { search ∷ String
  , users ∷ L.List String
  }
derive instance newtypeUsersState ∷ Newtype UsersState _

defaultUsersState ∷ UsersState
defaultUsersState = UsersState { search: "", users: L.Nil }

newtype GroupsState = GroupsState { }
derive instance newtypeGroupsState ∷ Newtype GroupsState _

defaultGroupsState ∷ GroupsState
defaultGroupsState = GroupsState { }

cpGroups :: forall f1 g p1 q. CP.ChildPath f1 (Coproduct f1 g) p1 (Either p1 q)
cpGroups = CP.cp1

data GroupItem = GroupItem { path ∷ QA.GroupPath, name ∷ String }

derive instance eqGroupItem ∷ Eq GroupItem
derive instance ordGroupItem ∷ Ord GroupItem

groupItemName ∷ GroupItem → String
groupItemName = case _ of
  GroupItem { name } → name

data GroupMessage
  = AddNewGroup { path ∷ QA.GroupPath, event ∷ DOM.Event, name ∷ String }
  | DeleteGroup { path ∷ QA.GroupPath }
