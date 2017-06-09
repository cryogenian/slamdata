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

module SlamData.AdminUI.Component
  ( component
  , Query(..)
  , State(..)
  , TabIndex(..)
  , MySettingsState(..)
  , DatabaseState(..)
  , ServerState(..)
  , GroupsState(..)
  , PostgresCon(..)
  , Group(..)
  , GroupPath
  ) where

import SlamData.Prelude

import Control.Comonad.Cofree (mkCofree)
import Data.Array as Array
import Data.List (List)
import Data.List as L
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Render.Icon as I
import SlamData.Workspace.MillerColumns.BasicItem.Component as MCI
import SlamData.Workspace.MillerColumns.Column.Component as MCC
import SlamData.Workspace.MillerColumns.Component as Miller
import SlamData.Workspace.MillerColumns.Component.State (ColumnsData)
import SlamData.Workspace.MillerColumns.TreeData as MCTree

data Query a
  = Init a
  | Open a
  | Close a
  | SetActive TabIndex a
  | SetMySettings MySettingsState a
  | SetDatabase DatabaseState a
  | SetServer ServerState a
  | SetGroups GroupsState a
  | HandleColumns (Miller.Message Group GroupPath) a

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

newtype GroupsState = GroupsState { tree ∷ MCTree.Tree Group }
derive instance newtypeGroupsState ∷ Newtype GroupsState _

defaultGroupsState ∷ GroupsState
defaultGroupsState = GroupsState { tree: mkCofree (Group { path: empty, name: "" }) L.Nil }

cpGroups = CP.cp1

newtype Group = Group { path ∷ List String, name ∷ String }
derive instance eqGroup ∷ Eq Group
derive instance ordGroup ∷ Ord Group
derive instance newtypeGroup ∷ Newtype Group _

type GroupPath = L.List String

type MillerQuery = Miller.Query Group GroupPath Void

type ChildQuery = MillerQuery ⨁ Const Void
type ChildSlot = Unit ⊹ Void

type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam
type DSL = H.ParentDSL State Query ChildQuery ChildSlot Void Slam

component ∷ H.Component HH.HTML Query Unit Void Slam
component =
  H.lifecycleParentComponent
    { initialState: \_ →
       { open: false
       , active: MySettings
       , formState:
          { mySettings: defaultMySettingsState
          , database: defaultDatabaseState
          , server: defaultServerState
          , groups: defaultGroupsState
          }
       }
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes $ fold
        [ pure (H.ClassName "sd-admin-ui")
        , guard (not state.open) $> H.ClassName "hidden"
        ]
    ]
    [ tabHeader state.active
    , tabBody state
    , HH.button
        [ HE.onClick (HE.input_ Close) ]
        [ HH.text "Close" ]
    ]

tabHeader ∷ TabIndex → HTML
tabHeader active =
  HH.ul
    [ HP.class_ $ H.ClassName "tabs"]
    $ Array.fromFoldable
    $ allTabs <#> \t →
      HH.li
        (fold
          [ pure $ HE.onClick $ HE.input_ $ SetActive t
          , guard (t == active) $> HP.class_ (H.ClassName "active-tab")
          ])
        [ HH.text (tabTitle t) ]

tabBody ∷ State → HTML
tabBody state =
  HH.form
    [HP.class_ $ HH.ClassName "tab-body"]
    case state.active of
      MySettings →
        pure $ HH.div
          [ HP.class_ (HH.ClassName "my-settings") ]
          (renderMySettingsForm state.formState.mySettings)
      Database →
        pure $ HH.div
          [ HP.class_ (HH.ClassName "database") ]
          (renderDatabaseForm state.formState.database)
      Server →
        pure $ HH.div
          [ HP.class_ (HH.ClassName "server") ]
          (renderServerForm state.formState.server)
      Groups →
        pure $ HH.div
          [ HP.class_ (HH.ClassName "groups") ]
          (renderGroupsForm state.formState.groups)
      _ →
        [HH.text "Not implemented"]
      -- Authentication → ?x
      -- Users → ?x

-- TODO(Christoph): Talk to Kyle
themes ∷ Array String
themes = ["Dark", "Light"]

renderMySettingsForm ∷ MySettingsState → Array HTML
renderMySettingsForm (MySettingsState state) =
    [ HH.fieldset
        [ HP.class_ (HH.ClassName "home-directory") ]
        [ HH.legend_ [ HH.text "Location of my home directory in the SlamData file system:" ]
        , HH.input
            [ HP.classes [ HH.ClassName "form-control" ]
            , HP.id_ "HomeDirectory"
            , HP.value state.homeDirectory
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "form-group" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "checkbox" ] ]
                [ HH.label_
                    [ HH.input
                        [ HP.checked state.isolateArtifacts
                        , HP.type_ HP.InputCheckbox
                        ]
                    , HH.text "Isolate SlamData artifacts to a specific location in the SlamData file system"
                    ]
                ]
            , HH.input
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.id_ "IsolateLocation"
                , HP.disabled (not state.isolateArtifacts)
                , HP.value state.isolateArtifactsDirectory
                ]
            , HH.p_
                [ HH.text $
                  fold
                  [ "If you choose this option, while you can still virtually locate decks anywhere inside the file system, "
                  , "they will always be physically stored in the above location. This allows you to keep production systems "
                  , "free of SlamData artifacts, while still centrally locating and backing them up."
                  ]
                ]
            ]
        ]
    , HH.fieldset
        [ HP.class_ (HH.ClassName "themes") ]
        [ HH.legend_ [ HH.text "Default theme for new decks:" ]
        , HH.div
            [ HP.class_ (HH.ClassName "theme-pickers") ]
            [ HH.select
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.id_ "ThemeSelection"
                ]
                (themes <#> \t → HH.option_ [HH.text t])
            , HH.select
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.id_ "ThemeSpacing"
                ]
                (themes <#> \t → HH.option_ [HH.text t])
            ]
        ]
    ]

renderDatabaseForm ∷ DatabaseState → Array HTML
renderDatabaseForm (DatabaseState state) =
  [ HH.fieldset
    [ HP.class_ (HH.ClassName "database-form-wrapper")]
    [ HH.legend
        [ HP.class_ (HH.ClassName "checkbox") ]
        [ HH.label_
            [ HH.input
                [ HP.checked (not state.isExternal)
                , HE.onChecked (HE.input_ (SetDatabase (DatabaseState (state {isExternal = false}) )))
                , HP.type_ HP.InputCheckbox
                ]
            , HH.text "Store SlamData metadata inside internal database in the local file system of the server"
            ]
        ]
    , HH.fieldset
        [ HP.class_ (HH.ClassName "internal-storage")
        , HP.disabled state.isExternal
        ]
        [ HH.input
            [ HP.classes [ HH.ClassName "form-control" ]
            , HP.value state.databaseFile
            ]
        ]
      , HH.legend
          [ HP.class_ (HH.ClassName "checkbox") ]
          [ HH.label_
              [ HH.input
                [ HP.checked state.isExternal
                , HE.onChecked (HE.input_ (SetDatabase (DatabaseState (state {isExternal = true}) )))
                , HP.type_ HP.InputCheckbox
                ]
              , HH.text "Store SlamData metadata inside external PostgreSQL"
              ]
          ]
      , HH.fieldset
          [ HP.class_ (HH.ClassName "external-storage")
          , HP.disabled (not state.isExternal)
          ]
          [ HH.div
              [ HP.class_ (HH.ClassName "form-group") ]
              [ HH.label [ HP.for "Server" ] [ HH.text "Server" ]
              , HH.input
                  [ HP.classes [ HH.ClassName "form-control" ]
                  , HP.id_ "Server"
                  , HP.value state.postgresCon.server
                  ]
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "form-group") ]
              [ HH.label [ HP.for "Port" ] [ HH.text "Port" ]
              , HH.input
                  [ HP.classes [ HH.ClassName "form-control" ]
                  , HP.id_ "Port"
                  , HP.value (show state.postgresCon.port)
                  ]
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "form-group") ]
              [ HH.label
                  [ HP.for "Username" ]
                  [ HH.text "Username"
                  , HH.input
                      [ HP.classes [ HH.ClassName "form-control" ]
                      , HP.id_ "Username"
                      , HP.value state.postgresCon.username
                      ]
                  ]
              , HH.label
                  [ HP.for "Password" ]
                  [ HH.text "Password"
                  , HH.input
                      [ HP.classes [ HH.ClassName "form-control" ]
                      , HP.id_ "Password"
                      , HP.value state.postgresCon.password
                      ]
                  ]
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "form-group") ]
              [ HH.label [ HP.for "Database" ] [HH.text "Database"] , HH.input
                  [ HP.classes [ HH.ClassName "form-control" ]
                  , HP.id_ "Database"
                  , HP.value state.postgresCon.database
                  ]
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "form-group") ]
              [ HH.label [ HP.for "Custom" ] [HH.text "Custom"]
              , HH.input
                  [ HP.classes [ HH.ClassName "form-control" ]
                  , HP.value (fst state.postgresCon.custom)
                  ]
              , HH.input
                  [ HP.classes [ HH.ClassName "form-control" ]
                  , HP.value (snd state.postgresCon.custom)
                  ]
              ]
          ]
      ]
  ]

renderServerForm ∷ ServerState → Array HTML
renderServerForm (ServerState state) =
  [ HH.fieldset_
        [ HH.legend_ [ HH.text "Port" ]
        , HH.input
            [ HP.class_ (HH.ClassName "form-control") ]
        , HH.p_ [ HH.text "Changing the port will restart the server and reload the browser to the new port. If there are any errors in changing to the new port, however, you may have to use the browser back button."
                ]
        ]
  , HH.fieldset_
        [ HH.legend_ [HH.text "Location of log file in the SlamData file system"]
        , HH.input
            [ HP.class_ (HH.ClassName "form-control") ]
        ]
  , HH.fieldset_
        [ HH.legend
            [ HP.class_ (HH.ClassName "checkbox") ]
            [ HH.label_
              [ HH.input
                [ HP.checked state.enableCustomSSL
                , HE.onChecked (HE.input_ (SetServer (ServerState (state {enableCustomSSL = not state.enableCustomSSL}))))
                , HP.type_ HP.InputCheckbox
                ]
              , HH.text "Enable Custom SSL"
              ]
            ]
        , HH.textarea [HP.class_ (HH.ClassName "form-control"), HP.disabled (not state.enableCustomSSL)]
        ]
  ]

renderGroupsForm ∷ GroupsState → Array HTML
renderGroupsForm (GroupsState _) =
  [ HH.slot' cpGroups unit (Miller.component columnOptions) columnState (HE.input (either HandleColumns absurd)) ]
  where
    -- columnState ∷ ColumnsData Group GroupPath
    -- columnState = lmap (_.path ∘ unwrap) (MCTree.initialStateFromTree (MCTree.constructTree go root elements))
    --   where
    --     go = ?x
    --     root = ?y
    --     elements = ?z
    columnState ∷ ColumnsData Group GroupPath
    columnState = Tuple L.Nil L.Nil

    columnOptions ∷ Miller.ColumnOptions Group GroupPath Void
    columnOptions =
      Miller.ColumnOptions
        { renderColumn: MCC.component
        , renderItem: MCI.component { label: _.name ∘ unwrap, render: renderItem }
        , label: _.name ∘ unwrap
        , isLeaf: const false
        , id: _.path ∘ unwrap
        }
    renderItem ∷ Group → MCI.BasicItemHTML
    renderItem (Group {name}) =
      HH.div
        [ HP.classes
            [ HH.ClassName "sd-miller-column-item-inner"
            , HH.ClassName "sd-miller-column-item-node"
            ]
        ]
        [ HH.span_ [ HH.text name]
        , I.chevronRightSm
        ]
    -- { renderColumn ∷ ColumnOptions a i o → i → ColumnComponent a i o
    -- , renderItem ∷ i → a → ItemComponent a o
    -- , label ∷ a → String
    -- , isLeaf ∷ i → Boolean
    -- , id ∷ a → i
    -- }

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    pure next
  Open next → do
    H.modify (_ { open = true })
    pure next
  Close next → do
    H.modify (_ { open = false })
    pure next
  SetActive ix next → do
    H.modify (_ { active = ix })
    pure next
  SetMySettings new next → do
    H.modify (_ { formState { mySettings = new } })
    pure next
  SetDatabase new next → do
    H.modify (_ { formState { database = new } })
    pure next
  SetServer new next → do
    H.modify (_ { formState { server = new } })
    pure next
  SetGroups new next → do
    H.modify (_ { formState { groups = new } })
    pure next
  HandleColumns columnMsg next → do
    case columnMsg of
      Miller.SelectionChanged _ _ _ →
        pure next
      Miller.LoadRequest req → do
        GroupsState { tree } ← H.gets _.formState.groups
        let res = MCTree.loadFromTree' (_.path ∘ unwrap) (_.name ∘ unwrap) tree req
        _ ← H.query' cpGroups unit (H.action (Miller.FulfilLoadRequest res))
        pure next
