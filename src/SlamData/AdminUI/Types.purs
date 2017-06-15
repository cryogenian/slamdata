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
import Data.Path.Pathy as Pathy
import Halogen as H
import Halogen.Component.ChildPath as CP
import Quasar.Advanced.Types as QA
import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Component as Miller

type MillerQuery = Miller.Query GroupItem GroupIndex GroupMessage

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
  | SetGroups GroupsState a
  | HandleColumns (Miller.Message GroupItem GroupIndex) a
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
  L.fromFoldable [MySettings, Database, Server, Authentication, Users, Groups]

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
      , groups ∷ GroupsState
      }
  }

newtype MySettingsState = MySettingsState
  { homeDirectory ∷ String
  , isolateArtifacts ∷ Boolean
  , isolateArtifactsDirectory ∷ String
  }

defaultMySettingsState ∷ MySettingsState
defaultMySettingsState =
  MySettingsState
    { homeDirectory: ""
    , isolateArtifacts: false
    , isolateArtifactsDirectory: ""
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
  , port: 1234
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

newtype GroupsState = GroupsState { }
derive instance newtypeGroupsState ∷ Newtype GroupsState _

defaultGroupsState ∷ GroupsState
defaultGroupsState = GroupsState { }

cpGroups :: forall f1 g p1 q. CP.ChildPath f1 (Coproduct f1 g) p1 (Either p1 q)
cpGroups = CP.cp1

data GroupItem
  = Group { path ∷ Pathy.AbsFile Pathy.Sandboxed, name ∷ String, isLeaf ∷ Boolean }
  | User { path ∷ Pathy.AbsFile Pathy.Sandboxed, id ∷ QA.UserId, name ∷ String }

derive instance eqGroupItem ∷ Eq GroupItem
derive instance ordGroupItem ∷ Ord GroupItem

groupItemName ∷ GroupItem → String
groupItemName = case _ of
  Group { name } → name
  User { name } → name

type GroupIndex = (Pathy.AbsFile Pathy.Sandboxed ⊹ Tuple QA.UserId (Pathy.AbsFile Pathy.Sandboxed)) ⊹ Pathy.AbsFile Pathy.Sandboxed

data GroupMessage = ToBeDone
