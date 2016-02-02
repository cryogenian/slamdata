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

module SlamData.Notebook.Menu.Component.State where

import Prelude

import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap

import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Component.State as HalogenMenu

import SlamData.Notebook.Component.State (NotebookShortcut())
import SlamData.Notebook.Menu.Component.Query
import SlamData.Notebook.Rename.Component as Rename

type StateP g = HalogenMenu.MenuP (Maybe Value) g

make :: (StrMap.StrMap NotebookShortcut) -> HalogenMenu.Menu (Maybe Value)
make shortcuts = HalogenMenu.makeMenu
  [ { label: "Notebook"
    , submenu:
        [ { label: "Rename/Move"
          , shortcutLabel: Nothing
          , value: Just $ renameQueryToValue $ Rename.Focus unit
          }
        , { label: "Delete"
          , shortcutLabel: Nothing
          , value: Nothing
          }
        , { label: "Publish"
          , shortcutLabel: StrMap.lookup "NotebookPublish" shortcuts >>= _.label
          , value: StrMap.lookup "NotebookPublish" shortcuts >>= _.value >>> pure
          }
        ]
    }
  , { label: "Insert"
    , submenu:
        [ { label: "Query"
          , shortcutLabel: StrMap.lookup "InsertQuery" shortcuts >>= _.label
          , value: StrMap.lookup "InsertQuery" shortcuts >>= _.value >>> pure
          }
        , { label: "Markdown"
          , shortcutLabel: StrMap.lookup "InsertMarkdown" shortcuts >>= _.label
          , value: StrMap.lookup "InsertMarkdown" shortcuts >>= _.value >>> pure
          }
        , { label: "Explore"
          , shortcutLabel: StrMap.lookup "InsertExplore" shortcuts >>= _.label
          , value: StrMap.lookup "InsertExplore" shortcuts >>= _.value >>> pure
          }
        , { label: "Search"
          , shortcutLabel: StrMap.lookup "InsertSearch" shortcuts >>= _.label
          , value: StrMap.lookup "InsertSearch" shortcuts >>= _.value >>> pure
          }
        , { label: "API"
          , shortcutLabel: StrMap.lookup "InsertAPI" shortcuts >>= _.label
          , value: StrMap.lookup "InsertAPI" shortcuts >>= _.value >>> pure
          }
        ]
    }
  , { label: "Cell"
    , submenu:
        [ { label: "Evaluate"
          , shortcutLabel: StrMap.lookup "CellEvaluate" shortcuts >>= _.label
          , value: StrMap.lookup "CellEvaluate" shortcuts >>= _.value >>> pure
          }
        , { label: "Delete"
          , shortcutLabel: Nothing
          , value: Nothing
          }
        ]
    }
  , { label: "Help"
    , submenu:
        [ { label: "Getting started"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/"
          }
        , { label: "Manual"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/front-end-manual/"
          }
        , { label: "SlamSQL reference"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/slamsql-reference/"
          }
        , { label: "SlamDown reference"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/slamdown-reference/"
          }
        , { label: "Cheatsheet"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/support/cheatsheet.pdf"
          }
        , { label: "How to guides"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/how-tos/"
          }
        , { label: "Securing access to SlamData"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/quick-guide-resources/"
          }
        , { label: "Report a bug"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "mailto:support@slamdata.com?subject=Bug found"
          }
        , { label: "Request support"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "mailto:support@slamdata.com?subject=Request help"
          }
        ]
    }
  ]
