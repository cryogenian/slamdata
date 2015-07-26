module View.Notebook.Cell (cell) where

import Prelude
import Data.Functor (($>))
import Controller.Notebook.Cell
import Controller.Notebook.Cell.Viz (insertViz)
import Css.Display
import Data.Array (length, null, catMaybes)
import Data.Date (Date(), toEpochMilliseconds)
import Data.String (indexOf)
import Data.Function (on)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Time (Milliseconds(..), Seconds(..), toSeconds)
import Input.Notebook (Input(..))
import Model.Action (isView)
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Port (Port(..))
import Model.Resource (resourceName)
import Optic.Core 
import View.Common
import View.Notebook.Cell.Ace (aceEditor)
import View.Notebook.Cell.Explore (exploreEditor, exploreOutput)
import View.Notebook.Cell.Markdown (markdownOutput)
import View.Notebook.Cell.Query (queryOutput)
import View.Notebook.Cell.Search (searchOutput, searchEditor)
import View.Notebook.Cell.Viz (vizChoices, vizOutput)
import View.Notebook.Common

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.CSS as CSS
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Model.Notebook.Cell.Query as Qu
import qualified View.Css as VC

cell :: forall e. State -> Cell -> Array (HTML e)
cell notebook cell =
  let outputContent = output notebook cell
  in [ H.div ([ A.classes $ [ B.containerFluid, VC.notebookCell, B.clearfix] ++
                            if cell ^. _hiddenEditor
                            then [VC.collapsed]
                            else [] ] <>
                            maybe [ ] viewingStyle (notebook ^. _viewingCell))
             $ catMaybes [ if not (notebook ^. _editable) then Nothing else Just $ header cell
                         , if hidden then Nothing else Just $ editor notebook cell
                         , Just $ statusBar notebook (isJust outputContent) cell
                         , outputContent
                         ]
     ]
  where
  hidden = (cell ^. _hiddenEditor) || (not (notebook ^. _editable))

  viewingStyle cellId =
    if cellId == cell ^. _cellId
    then [ ]
    else [ CSS.style (display displayNone) ]

header :: forall e. Cell -> HTML e
header cell =
  let info = cellInfo cell
  in H.div [ A.classes [VC.cellHeader, B.clearfix] ]
           [ H.div [ A.class_ VC.cellIcon ]
                   [ glyph info.glyph ]
           , H.div [ A.class_ VC.cellName ]
                   [ H.text info.name ]
           , controls cell
           ]

controls :: forall e. Cell -> HTML e
controls cell =
  H.div [ A.classes [B.pullRight, VC.cellControls] ]
        [ H.button [ A.title if cell ^. _hiddenEditor
                             then "Show cell options"
                             else "Hide cell options"
                   , E.onClick $ E.input_ $ UpdateCell (cell ^. _cellId) (_hiddenEditor %~ not) ]
                   [ glyph if cell ^. _hiddenEditor
                           then B.glyphiconEyeOpen
                           else B.glyphiconEyeClose
                   ]
        , H.button [ A.title "Delete cell"
                   , E.onClick $ E.input_ $ TrashCell $ cell ^. _cellId ]
                   [ glyph B.glyphiconTrash ]
        , glyph B.glyphiconChevronLeft
        ]

output :: forall e. State -> Cell -> Maybe (HTML e)
output notebook cell =
  let out = renderOutput cell
  in if null out
     then Nothing
     else Just $ H.div [ A.classes [B.row, VC.cellOutput] ]
                       [ H.div [ A.class_ VC.cellOutputLabel ]
                               $ cellOutputBar notebook cell
                       , H.div [ A.class_ VC.cellOutputResult ]
                               out
                       ]

cellOutputBar :: forall e. State -> Cell -> Array (HTML e)
cellOutputBar notebook cell =
  case cell ^. _output of
    PortResource res ->
      [ H.text $ resourceName res ++ " := "
      , H.ul [ A.classes [VC.nextCellList] ]
             [ li "Query this output" (E.input_ $ InsertCell cell $ newQueryContent res) B.glyphiconHdd
             , li "Search this output" (E.input_ $ InsertCell cell $ newSearchContent res) B.glyphiconSearch
             , li "Visualize this output" (\_ -> pure $ insertViz notebook cell) B.glyphiconPicture
             ]
      ]

    VarMap _ ->
      [ H.ul [ A.classes [VC.nextCellList] ]
             [ li "Query using fields" (E.input_ $ InsertCell cell $ Query Qu.initialQueryRec) B.glyphiconHdd
             ]
      ]

    _ -> []
  where
  li :: String -> _ -> A.ClassName -> HTML e
  li title handler c =
    H.li_ [ H.button [A.title title, E.onClick handler] [glyph c] ]

statusBar :: forall e. State -> Boolean -> Cell -> HTML e
statusBar notebook hasOutput cell =
  row' (fadeWhen (cell ^. _hiddenEditor))
  $ if (cell ^. _hiddenEditor)
    then []
    else [ H.div [ A.classes [VC.cellEvalLine, B.clearfix] ]
           $ [ H.button [ A.classes [B.btn, B.btnPrimary, buttonClass]
                        , E.onClick \_ -> pure $ handleRunClick (notebook ^._notebook) cell
                        ]
               [ glyph buttonGlyph ]
             , H.div [ A.classes [ VC.statusText ] ]
               [ H.text $ statusText notebook.tickDate (cell ^. _runState) ]
             , H.div [ A.classes [ B.pullRight, VC.cellControls ] ]
               $ catMaybes [ refreshButton
                           , toggleMessageButton
                           , linkButton
                           , Just $ glyph B.glyphiconChevronLeft
                           ]
             ] ++ messages
         ]
  where
  buttonClass :: A.ClassName
  buttonClass = if isRunning cell then VC.stopButton else VC.playButton

  buttonGlyph :: A.ClassName
  buttonGlyph = if isRunning cell then B.glyphiconStop else B.glyphiconPlay

  messages :: Array (HTML e)
  messages = message cell

  toggleMessageButton :: Maybe (HTML e)
  toggleMessageButton =
    if null messages
    then Nothing
    else Just $ H.button [ A.title if cell ^._expandedStatus
                                   then "Hide messages"
                                   else "Show messages"
                         , E.onClick (\_ -> E.preventDefault $> pure (UpdateCell (cell ^._cellId) (_expandedStatus %~ not))) ]
         [ glyph if cell ^._expandedStatus
                 then B.glyphiconEyeClose
                 else B.glyphiconEyeOpen ]

  linkButton :: Maybe (HTML e)
  linkButton =
    if not hasOutput
    then Nothing
    else Just $ H.button [ A.title "Embed cell output"
                         , E.onClick (\_ -> pure $ handleEmbedClick notebook cell)
                         , E.onMouseEnter $ E.input_ (UpdateCell (cell ^. _cellId) (_embedHovered .~ true))
                         , E.onMouseLeave $ E.input_ (UpdateCell (cell ^. _cellId) (_embedHovered .~ false))
                         ]
         [ H.img [ A.src if (cell ^. _embedHovered)
                         then "img/code-icon-blue.svg"
                         else "img/code-icon.svg"
                 , A.width 16.0 ] [] ]

  refreshButton :: Maybe (HTML e)
  refreshButton =
    Just $ H.button [ A.title "Refresh cell content"
                    , A.classes [VC.refreshButton]
                    , E.onClick (\_ -> pure $ handleRefreshClick cell)
                    ]
    [ glyph B.glyphiconRefresh ]

message :: forall e. Cell -> Array (HTML e)
message cell =
  let collapsed = if cell ^._expandedStatus then [] else [VC.collapsed]
  in if null (cell ^._failures)
     then if (cell ^._message) /= ""
          then [ H.div [ A.classes $ [VC.cellMessages] ++ collapsed ]
                       $ if cell ^._expandedStatus
                         then [ H.div_ []
                              , messageText cell
                              ]
                         else []
               ]
          else [ ]
     else [ H.div [ A.classes $ [VC.cellFailures] ++ collapsed ]
                              $ failureText cell
          ]

details :: forall e. Cell -> Array (HTML e)
details cell = commonMessage "" [H.div_ [H.text (cell^._message)]] cell

messageText :: forall e. Cell -> HTML e
messageText cell =
  if isJust $ indexOf "\n" m
  then H.pre_ [H.text m]
  else H.div_ [H.text m]
  where m = cell ^._message

failureText :: forall e. Cell -> Array (HTML e)
failureText cell =
  commonMessage
  (show (length fs) <> " error(s) during evaluation. ")
  ((\f -> if isJust $ indexOf "\n" f
          then H.pre_ [H.text f]
          else H.div_ [H.text f]) <$> fs)
  cell
  where fs = cell ^._failures

commonMessage :: forall e. String -> Array (HTML e) -> Cell -> Array (HTML e)
commonMessage intro children cell =
  [ H.div_ [ H.text intro ]
  ] <>
  if (cell ^._expandedStatus)
  then children
  else [ ]

statusText :: Maybe Date -> RunState -> String
statusText _ RunInitial = ""
statusText d (RunningSince d') =
  maybe "" (\s -> "Running for " <> s <> "s") $ ((flip secondsText d') <$> d)
statusText _ (RunFinished (Milliseconds ms)) = "Finished: took " <> show ms <> "ms."

secondsText :: Date -> Date -> String
secondsText a b = show <<< max 0 <<<
                  unSeconds $ on (-) (toSeconds <<< toEpochMilliseconds) a b
  where
  max :: forall a. (Ord a) => a -> a -> a
  max a b = if a > b then a else b
  unSeconds (Seconds n) = n

cellInfo :: Cell -> { name :: String, glyph :: A.ClassName }
cellInfo cell = case cell ^. _content of
  Explore _ -> { name: "Explore", glyph: B.glyphiconEyeOpen }
  Markdown _ -> { name: "Markdown", glyph: B.glyphiconEdit }
  Search _ -> { name: "Search", glyph: B.glyphiconSearch }
  Visualize _ -> { name: "Visualize", glyph: B.glyphiconPicture }
  Query _ -> { name: "Query", glyph: B.glyphiconHdd }

editor :: forall e. State -> Cell -> HTML e
editor state cell = case cell ^. _content of
  Explore rec -> exploreEditor cell
  Search _ -> searchEditor (state ^._notebook) cell
  Visualize _ -> vizChoices cell
  _ -> aceEditor cell

renderOutput :: forall e. Cell -> Array (HTML e)
renderOutput cell =
  if not (null $ cell ^. _failures)
  then []
  else case cell ^. _content of
    Explore _ -> exploreOutput cell
    Markdown mr -> markdownOutput mr cell
    Search _ -> searchOutput cell
    Visualize _ -> vizOutput cell
    Query _ -> queryOutput cell
