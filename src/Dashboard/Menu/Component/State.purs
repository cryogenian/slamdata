module Dashboard.Menu.Component.State where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Component.State as HalogenMenu
import Data.StrMap as StrMap

import Model.CellType (CellType(..))
import Notebook.Component as Notebook
import Dashboard.Component.State (NotebookShortcut())
import Dashboard.Menu.Component.Query

type StateP g = HalogenMenu.MenuP (Maybe Value) g

make :: (StrMap.StrMap NotebookShortcut) -> HalogenMenu.Menu (Maybe Value)
make shortcuts = HalogenMenu.makeMenu
  [ { label: "Notebook"
    , submenu:
        [ { label: "Rename/Move"
          , shortcutLabel: Nothing
          , value: Just $ notebookQueryToValue $ (Notebook.AddCell Explore) unit
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

