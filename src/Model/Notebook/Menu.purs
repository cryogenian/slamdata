module Model.Notebook.Menu where

import Data.Either
import Data.Maybe
import Data.Int (Int(), fromNumber)
import Data.Inject1 (inj)

data MenuNotebookSignal
  = RenameNotebook
  | DeleteNotebook
  | PublishNotebook

data MenuEditSignal
  = CopyEdit
  | PasteEdit
  | CutEdit

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
                 (Either MenuEditSignal
                  (Either MenuInsertSignal
                   (Either MenuCellSignal
                    MenuHelpSignal)))



type MenuElement =
  { name :: String
  , message :: Maybe MenuSignal
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
      [ { name: "Rename/Move (Cmd + Shift + S)"
        , message: Just $ inj RenameNotebook
        , lvl: fromNumber 0 }
      , { name: "Delete"
        , message: Just $ inj DeleteNotebook
        , lvl: fromNumber 0}
      , { name: "Publish (Cmd + P)"
        , message: Just $ inj PublishNotebook
        , lvl: fromNumber 0}
      ]
    }
  , { visible: false
    , name: "Edit"
    , children:
      [ { name: "Copy (Cmd + C)"
        , message: Just $ inj CopyEdit
        , lvl: fromNumber 0 }
      , { name: "Cut (Cmd + X)"
        , message: Just $ inj CutEdit
        , lvl: fromNumber 0 }
      , { name: "Paste (Cmd + V)"
        , message: Just $ inj PasteEdit
        , lvl: fromNumber 0 }]}
  , { visible: false
    , name: "Insert"
    , children:
      [ { name: "Cell"
        , message: Nothing
        , lvl: fromNumber 0 }
      , { name: "Query (Cmd + 1)"
        , message: Just $ inj QueryInsert
        , lvl: fromNumber 1 }
      , { name: "Markdown (Cmd + 2)"
        , message: Just $ inj MarkdownInsert
        , lvl: fromNumber 1 }
      , { name: "Explore (Cmd + 3)"
        , message: Just $ inj ExploreInsert
        , lvl: fromNumber 1 }
      , { name: "Search (Cmd + 4)"
        , message: Just $ inj SearchInsert
        , lvl: fromNumber 1 }
       ] }
  , { visible: false
    , name: "Cell"
    , children:
      [ { name: "Evaluate (Cmd + Enter)"
        , message: Just $ inj $ EvaluateCell
        , lvl: fromNumber 0 }
      , { name: "Delete"
        , message: Just $ inj $ DeleteCell
        , lvl: fromNumber 0 }
      , { name: "Split"
        , message: Nothing
        , lvl: fromNumber 0 }
      , { name: "2 Column"
        , message: Just $ inj $ SplitCell (fromNumber 2)
        , lvl: fromNumber 1 }
      , { name: "3 Column"
        , message: Just $ inj $ SplitCell (fromNumber 3)
        , lvl: fromNumber 1 }
      , { name: "4 Column"
        , message: Just $ inj $ SplitCell (fromNumber 4)
        , lvl: fromNumber 1 }
      , { name: "5 Column"
        , message: Just $ inj $ SplitCell (fromNumber 5)
        , lvl: fromNumber 1 } ] }
  , { visible: false
    , name: "Help"
    , children:
      [ { name: "Tutorial"
        , message: Just $ inj $ TutorialHelp
        , lvl: fromNumber 0 }
      , { name: "SQL Tutorial"
        , message: Just $ inj $ SQLTutorialHelp
        , lvl: fromNumber 0 }
      , { name: "SQL Reference"
        , message: Just $ inj $ SQLReferenceHelp
        , lvl: fromNumber 0 }
      , { name: "Report bug"
        , message: Just $ inj $ ReportBugHelp
        , lvl: fromNumber 0 }
      , { name: "Request support"
        , message: Just $ inj $ RequestSupportHelp
        , lvl: fromNumber 0 }]}
  ]

