module View.Notebook.Cell (cell) where

import Control.Functor (($>))
import Controller.Notebook.Cell
import Controller.Notebook.Cell.Viz (insertViz)
import Data.Array (length, null, catMaybes)
import Data.Date (Date(), toEpochMilliseconds)
import Data.Function (on)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Time (Milliseconds(..), Seconds(..), toSeconds)
import Input.Notebook (Input(..))
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Port (Port(..))
import Model.Resource (resourceName)
import Number.Format (toFixed)
import Optic.Core ((^.), (%~), (..), is)
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
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC

cell :: forall e. State -> Cell -> [HTML e]
cell notebook cell =
  let outputContent = output notebook cell
  in [ H.div [ A.classes $ [B.containerFluid, VC.notebookCell, B.clearfix] ++ if cell ^. _hiddenEditor then [VC.collapsed] else [] ]
             $ catMaybes [ Just $ header cell
                         , if cell ^. _hiddenEditor then Nothing else Just $ editor cell
                         , if cell ^. _hiddenEditor then Nothing else Just $ statusBar notebook (isJust outputContent) cell
                         , outputContent
                         ]
     ]

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

cellOutputBar :: forall e. State -> Cell -> [HTML e]
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

    _ -> []
  where
  li :: String -> _ -> A.ClassName -> HTML e
  li title handler c =
    H.li_ [ H.button [A.title title, E.onClick handler] [glyph c] ]

statusBar :: forall e. State -> Boolean -> Cell -> HTML e
statusBar notebook hasOutput cell =
  let buttonClass = if isRunning cell then VC.stopButton else VC.playButton
      buttonGlyph = if isRunning cell then B.glyphiconStop else B.glyphiconPlay
      messages = message cell
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
      linkButton =
        if not hasOutput
        then Nothing
        else Just $ H.button [ A.title "Share cell output"
                             , E.onClick (\_ -> pure $ handleShareClick notebook cell)
                             ]
                             [ glyph B.glyphiconShare ]
  in row' (fadeWhen (cell ^. _hiddenEditor))
     [ H.div [ A.classes [VC.cellEvalLine, B.clearfix] ]
             $ [ H.button [ A.classes [B.btn, B.btnPrimary, buttonClass]
                          , E.onClick \_ -> pure $ handleRunClick cell
                          ]
                          [ glyph buttonGlyph ]
               , H.div [ A.classes [ VC.statusText ] ]
                       [ H.text $ statusText notebook.tickDate (cell ^. _runState) ]
               , H.div [ A.classes [ B.pullRight, VC.cellControls ] ]
                       $ catMaybes [ toggleMessageButton
                                   , linkButton
                                   ]
               ] ++ messages
     ]

message :: forall e. Cell -> [HTML e]
message cell =
  let collapsed = if cell ^._expandedStatus then [] else [VC.collapsed]
  in if null (cell ^._failures)
     then if (cell ^._message) /= ""
          then [ H.div [ A.classes $ [VC.cellMessages] ++ collapsed ]
                       $ if cell ^._expandedStatus
                         then [ H.div_ []
                              , H.div_ [ H.text $ cell ^._message ]
                              ]
                         else []
               ]
          else [ ]
     else [ H.div [ A.classes $ [VC.cellFailures] ++ collapsed ]
                              $ failureText cell
          ]

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

statusText :: Maybe Date -> RunState -> String
statusText _ RunInitial = ""
statusText d (RunningSince d') = maybe "" (\s -> "Running for " <> s <> "s") $ d >>= flip secondsText d'
statusText _ (RunFinished (Milliseconds ms)) = "Finished: took " <> show ms <> "ms."

secondsText :: Date -> Date -> Maybe String
secondsText a b = toFixed 0 <<< Math.max 0 <<< unSeconds $ on (-) (toSeconds <<< toEpochMilliseconds) a b
  where unSeconds (Seconds n) = n

cellInfo :: Cell -> { name :: String, glyph :: A.ClassName }
cellInfo cell = case cell ^. _content of
  Explore _ -> { name: "Explore", glyph: B.glyphiconEyeOpen }
  Markdown _ -> { name: "Markdown", glyph: B.glyphiconEdit }
  Search _ -> { name: "Search", glyph: B.glyphiconSearch }
  Visualize _ -> { name: "Visualize", glyph: B.glyphiconPicture }
  Query _ -> { name: "Query", glyph: B.glyphiconHdd }

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
    Markdown s -> markdownOutput s (cell ^. _cellId)
    Search _ -> searchOutput cell
    Visualize _ -> vizOutput cell
    Query _ -> queryOutput cell
