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

module Model.Notebook.Menu where

import Prelude
import Data.Either
import Data.Maybe
import Data.Inject1 (inj)
import Data.KeyCombo

import qualified Data.String.Unsafe as U

data MenuNotebookSignal
  = RenameNotebook
  | DeleteNotebook
  | PublishNotebook

data MenuInsertSignal
  = ExploreInsert
  | MarkdownInsert
  | QueryInsert
  | SearchInsert

data MenuCellSignal
  = EvaluateCell
  | DeleteCell
  | SplitCell Int

data MenuHelpSignal
  = TutorialHelp
  | SQLTutorialHelp
  | SQLReferenceHelp
  | ReportBugHelp
  | RequestSupportHelp

type MenuSignal = Either MenuNotebookSignal
                  (Either MenuInsertSignal
                   (Either MenuCellSignal
                    MenuHelpSignal))

type MenuElement =
  { name :: String
  , message :: Maybe MenuSignal
  , shortcut :: Maybe KeyCombo
  , lvl :: Int }

type DropdownItem =
  { name :: String
  , visible :: Boolean
  , children :: Array MenuElement 
  }

initialDropdowns :: Array DropdownItem
initialDropdowns =
  [ { visible: false
    , name: "Notebook"
    , children:
      [ { name: "Rename/Move"
        , message: Just $ inj RenameNotebook
        , shortcut: Just $ meta ++ shift ++ letter (U.charAt 0 "S")
        , lvl:  0 }
      , { name: "Delete"
        , message: Just $ inj DeleteNotebook
        , shortcut: Nothing
        , lvl:  0}
      , { name: "Publish"
        , message: Just $ inj PublishNotebook
        , shortcut: Just $ meta ++ letter (U.charAt 0 "P")
        , lvl:  0}
      ]
    }
  , { visible: false
    , name: "Insert"
    , children:
      [ { name: "Query"
        , message: Just $ inj QueryInsert
        , shortcut: Just $ meta ++ letter (U.charAt 0 "1")
        , lvl:  0 }
      , { name: "Markdown"
        , message: Just $ inj MarkdownInsert
        , shortcut: Just $ meta ++ letter (U.charAt 0 "2")
        , lvl:  0 }
      , { name: "Explore"
        , message: Just $ inj ExploreInsert
        , shortcut: Just $ meta ++ letter (U.charAt 0 "3")
        , lvl:  0 }
      , { name: "Search"
        , message: Just $ inj SearchInsert
        , shortcut: Just $ meta ++ letter (U.charAt 0 "4")
        , lvl:  0 }
       ] }
  , { visible: false
    , name: "Cell"
    , children:
      [ { name: "Evaluate"
        , message: Just $ inj $ EvaluateCell
        , shortcut: Just $ meta ++ enter
        , lvl:  0 }
      , { name: "Delete"
        , message: Just $ inj $ DeleteCell
        , shortcut: Nothing
        , lvl:  0 }
      ] }
  , { visible: false
    , name: "Help"
    , children:
      [ { name: "Tutorial"
        , message: Just $ inj $ TutorialHelp
        , shortcut: Nothing
        , lvl:  0 }
      , { name: "SQL Tutorial"
        , message: Just $ inj $ SQLTutorialHelp
        , shortcut: Nothing
        , lvl:  0 }
      , { name: "SQL Reference"
        , message: Just $ inj $ SQLReferenceHelp
        , shortcut: Nothing
        , lvl:  0 }
      , { name: "Report bug"
        , message: Just $ inj $ ReportBugHelp
        , shortcut: Nothing
        , lvl:  0 }
      , { name: "Request support"
        , message: Just $ inj $ RequestSupportHelp
        , shortcut: Nothing
        , lvl:  0 }]}
  ]
