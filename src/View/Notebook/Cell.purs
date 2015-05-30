module View.Notebook.Cell (cell) where

import Control.Functor (($>))
import Controller.Notebook.Cell (requestCellContent)
import Controller.Notebook.Cell.Viz (insertViz)
import Data.Array (length, null)
import Data.Date (Date(), toEpochMilliseconds)
import Data.Function (on)
import Data.Maybe (Maybe(), maybe)
import Data.Time (Milliseconds(..), Seconds(..), toSeconds)
import Input.Notebook (Input(..))
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Port (Port(..))
import Number.Format (toFixed)
import Optic.Core ((^.), (%~), (..), is)

import View.Common
import View.Notebook.Cell.Ace (aceEditor)
import View.Notebook.Cell.Explore (exploreEditor, exploreOutput)
import View.Notebook.Cell.Search (searchOutput, searchEditor)
import View.Notebook.Cell.Viz (vizChoices, vizOutput)
import View.Notebook.Cell.Markdown (markdownOutput)
import View.Notebook.Cell.Query (queryOutput)
import View.Notebook.Common

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC

cell :: forall e. State -> Cell -> [HTML e]
cell notebook state =
  [ H.div [ A.classes [B.containerFluid, VC.notebookCell] ]
    [ controls state
    , editor state
    , statusBar notebook.tickDate state
    , output state
    , insertNextCell notebook state
    ]
  ]

controls :: forall e. Cell -> HTML e
controls state =
  row [ H.div [ A.classes [B.btnGroup, B.pullRight, VC.cellControls] ]
        [ H.button [ A.classes [ B.btn
                               , B.glyphicon
                               , if state ^. _hiddenEditor
                                 then B.glyphiconEyeOpen
                                 else B.glyphiconEyeClose
                               ]
                   , E.onClick $ E.input_ $ UpdateCell (state ^. _cellId) (_hiddenEditor %~ not)
                   ]
          [ ]
        , H.button [ A.classes [ B.btn
                               , B.glyphicon
                               , B.glyphiconTrash
                               ]
                   , E.onClick $ E.input_ $ TrashCell $ state ^. _cellId
                   ]
          [ ]
        ]
      ]

output :: forall e. Cell -> HTML e
output state =
   H.div [ A.classes $ [B.row, VC.cellOutput] ]
   $ renderOutput state

statusBar :: forall e. Maybe Date -> Cell -> HTML e
statusBar d state =
  row' (fadeWhen (state ^. _hiddenEditor))
  [ H.div [ A.classes [VC.cellEvalLine, B.clearfix] ]
    [ H.button [ A.classes [ B.btn
                           , B.btnPrimary
                           , if isRunning (state ^. _runState)
                             then VC.stopButton
                             else VC.playButton ]
               , E.onClick \_ -> if isRunning (state ^. _runState)
                                 then pure (pure (StopCell $ state ^. _cellId))
                                 else pure (requestCellContent state)
               ]
      [ if isRunning (state ^. _runState)
        then glyph B.glyphiconStop
        else glyph B.glyphiconPlay ]
    , H.div [ A.classes [ VC.statusText ] ]
      [ H.text $ statusText d (state ^. _runState) ]
    , H.div [ A.classes [ B.pullRight ] ]
      [ H.button [ A.classes [ B.btn
                             , B.glyphicon
                             , if state ^._expandedStatus
                               then B.glyphiconEyeClose
                               else B.glyphiconEyeOpen
                             ]
                 , E.onClick (\_ -> E.preventDefault $>
                                   pure (UpdateCell (state ^._cellId)
                                         (_expandedStatus %~ not)))
                 ]
                 [ ]
      ]
    , H.div [ A.classes [ VC.cellFailures ] ]
      (message state)
    ]
  ]

insertNextCell :: forall e. State -> Cell -> HTML e
insertNextCell notebook state = row
  [ H.div [ A.classes [ VC.cellEvalLine, B.clearfix ] ]
    [ H.button
      [ A.classes [ B.btn
                  , B.btnPrimary
                  , VC.playButton
                  ]
      , E.onClick $ E.input_ $ UpdateCell (state ^. _cellId) (_addingNextCell %~ not)
      ]
      [ glyph if state ^. _addingNextCell
              then B.glyphiconMenuLeft
              else B.glyphiconMenuRight ]
    , H.ul [ A.classes ([ B.listInline, VC.nextCellList ] <>
                        (fadeWhen $ not $ (state ^. _addingNextCell))) ]
      (nextCellChoices notebook state)
    ]
  ]
nextCellChoices :: forall e. State -> Cell -> [HTML e]
nextCellChoices notebook state =
  [ li "Query cell" newQueryContent  B.glyphiconHdd
  , li "Explore cell" newExploreContent B.glyphiconEyeOpen
  , li "Markdown cell" newMarkdownContent B.glyphiconEdit
  , li "Search cell" newSearchContent B.glyphiconSearch] <>
  case state ^. _output of
    PortResource _ ->
      [ H.li_ [ H.a [ A.href "#"
                  , E.onClick (\_ -> E.preventDefault $> insertViz notebook state)
                  , A.title "Visualization cell"
                  , A.classes [ B.btn ]
                  ]
              [ glyph B.glyphiconPicture ]
              ]
      ]
    _ -> [ ]


  where
  li :: String -> CellContent -> A.ClassName -> HTML e
  li title content c =
    H.li_ [ H.a [ A.href "#"
                , E.onClick \_ -> E.preventDefault $> pure (InsertCell state content)
                , A.title title
                , A.classes [ B.btn ] ] [ glyph c ] ]

message :: forall e. Cell -> [HTML e]
message cell =
  if null (cell ^._failures)
  then if (cell ^._message) /= ""
       then details cell
       else [ ]
  else failureText cell

details :: forall e. Cell -> [HTML e]
details cell = commonMessage "" [H.div_ [H.text (cell^._message)]] cell

failureText :: forall e. Cell -> [HTML e]
failureText cell =
  commonMessage
  (show (length fs) <> " error(s) during evaluation. ")
  ((\f -> H.div_ [H.text f]) <$> fs)
  cell
  where fs = cell ^._failures

commonMessage :: forall e. String -> [HTML e] -> Cell -> [HTML e]
commonMessage intro children cell = 
  [ H.div_ [ H.text intro ]
  ] <>
  if (cell ^._expandedStatus)
  then children
  else [ ]

isRunning :: RunState -> Boolean
isRunning (RunningSince _) = true
isRunning _ = false

statusText :: Maybe Date -> RunState -> String
statusText _ RunInitial = ""
statusText d (RunningSince d') = maybe "" (\s -> "Running for " <> s <> "s") $ d >>= flip secondsText d'
statusText _ (RunFinished (Milliseconds ms)) = "Finished: took " <> show ms <> "ms."

secondsText :: Date -> Date -> Maybe String
secondsText a b = toFixed 0 <<< Math.max 0 <<< unSeconds $ on (-) (toSeconds <<< toEpochMilliseconds) a b
  where unSeconds (Seconds n) = n

editor :: forall e. Cell -> HTML e
editor state = case state ^. _content of
  (Explore rec) -> exploreEditor state
  (Search _) -> searchEditor state
  (Visualize _) -> vizChoices state
  _ -> aceEditor state


renderOutput :: forall e. Cell -> [HTML e]
renderOutput cell =
  if not (null $ cell ^. _failures)
  then []
  else case cell ^. _content of
    Explore _ -> exploreOutput cell
    Markdown s -> [markdownOutput s (cell ^. _cellId)]
    Search _ -> searchOutput cell
    Visualize _ -> vizOutput cell
    Query _ -> queryOutput cell
    _ -> []
