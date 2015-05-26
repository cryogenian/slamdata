module Model.Notebook.Menu where

import Data.Either
import Data.Maybe
import Data.Int (Int(), fromNumber)
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
  , children :: [ MenuElement ]
  }

initialDropdowns :: [DropdownItem]
initialDropdowns =
  [ { visible: false
    , name: "Notebook"
    , children:
      [ { name: "Rename/Move"
        , message: Just $ inj RenameNotebook
        , shortcut: Just $ meta ++ shift ++ letter (U.charAt 0 "S")
        , lvl: fromNumber 0 }
      , { name: "Delete"
        , message: Just $ inj DeleteNotebook
        , shortcut: Nothing
        , lvl: fromNumber 0}
      , { name: "Publish"
        , message: Just $ inj PublishNotebook
        , shortcut: Just $ meta ++ letter (U.charAt 0 "P")
        , lvl: fromNumber 0}
      ]
    }
  , { visible: false
    , name: "Insert"
    , children:
      [ { name: "Query"
        , message: Just $ inj QueryInsert
        , shortcut: Just $ meta ++ letter (U.charAt 0 "1")
        , lvl: fromNumber 0 }
      , { name: "Markdown"
        , message: Just $ inj MarkdownInsert
        , shortcut: Just $ meta ++ letter (U.charAt 0 "2")
        , lvl: fromNumber 0 }
      , { name: "Explore"
        , message: Just $ inj ExploreInsert
        , shortcut: Just $ meta ++ letter (U.charAt 0 "3")
        , lvl: fromNumber 0 }
      , { name: "Search"
        , message: Just $ inj SearchInsert
        , shortcut: Just $ meta ++ letter (U.charAt 0 "4")
        , lvl: fromNumber 0 }
       ] }
  , { visible: false
    , name: "Cell"
    , children:
      [ { name: "Evaluate"
        , message: Just $ inj $ EvaluateCell
        , shortcut: Just $ meta ++ enter
        , lvl: fromNumber 0 }
      , { name: "Delete"
        , message: Just $ inj $ DeleteCell
        , shortcut: Nothing
        , lvl: fromNumber 0 }
      ] }
  , { visible: false
    , name: "Help"
    , children:
      [ { name: "Tutorial"
        , message: Just $ inj $ TutorialHelp
        , shortcut: Nothing
        , lvl: fromNumber 0 }
      , { name: "SQL Tutorial"
        , message: Just $ inj $ SQLTutorialHelp
        , shortcut: Nothing
        , lvl: fromNumber 0 }
      , { name: "SQL Reference"
        , message: Just $ inj $ SQLReferenceHelp
        , shortcut: Nothing
        , lvl: fromNumber 0 }
      , { name: "Report bug"
        , message: Just $ inj $ ReportBugHelp
        , shortcut: Nothing
        , lvl: fromNumber 0 }
      , { name: "Request support"
        , message: Just $ inj $ RequestSupportHelp
        , shortcut: Nothing
        , lvl: fromNumber 0 }]}
  ]
