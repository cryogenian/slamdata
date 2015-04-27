module Model.Notebook.Menu where

import Data.Either
import Data.Maybe
import Data.Int (Int(), fromNumber)
import Data.Inject1 (inj)

data MenuNotebookSignal
  = RenameNotebook
  | MoveNotebook
  | DeleteNotebook

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
      [ { name: "Rename"
        , message: Just $ inj RenameNotebook
        , lvl: fromNumber 0 }
      , { name: "Move"
        , message: Just $ inj MoveNotebook
        , lvl: fromNumber 0 }
      , { name: "Delete"
        , message: Just $ inj DeleteNotebook
        , lvl: fromNumber 0}
      ]
    }
  , { visible: false
    , name: "Edit"
    , children:
      [ { name: "Copy"
        , message: Just $ inj CopyEdit
        , lvl: fromNumber 0 }
      , { name: "Cut"
        , message: Just $ inj CutEdit
        , lvl: fromNumber 0 }
      , { name: "Paste"
        , message: Just $ inj PasteEdit
        , lvl: fromNumber 0 }]}
  , { visible: false
    , name: "Insert"
    , children:
      [ { name: "Cell"
        , message: Nothing
        , lvl: fromNumber 0 }
      , { name: "Explore"
        , message: Just $ inj ExploreInsert
        , lvl: fromNumber 1 }
      , { name: "Markdown"
        , message: Just $ inj MarkdownInsert
        , lvl: fromNumber 1 }
      , { name: "Query"
        , message: Just $ inj QueryInsert
        , lvl: fromNumber 1 }
      , { name: "Search"
        , message: Just $ inj SearchInsert
        , lvl: fromNumber 1 } ] }
  , { visible: false
    , name: "Cell"
    , children:
      [ { name: "Evaluate"
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

