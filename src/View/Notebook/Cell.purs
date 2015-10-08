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

module View.Notebook.Cell (cell) where

import Prelude
import Data.Functor (($>))
import Controller.Notebook.Cell
import Controller.Notebook.Cell.Viz (insertViz)
import Css.Display
import Data.Array (length, null, catMaybes)
import Data.BrowserFeatures (BrowserFeatures())
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
import Data.Int (fromNumber)
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
import qualified Utils.Halide as A
import qualified Halogen.HTML.CSS as CSS
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Model.Notebook.Cell.Query as Qu
import qualified View.Css as VC

cell :: forall e. State -> Cell -> Array (HTML e)
cell notebook c =
  let outputContent = output notebook c
  in [ H.div ([ A.classes $ [ B.containerFluid, VC.notebookCell, B.clearfix] ++
                            if c ^. _hiddenEditor
                            then [VC.collapsed]
                            else [] ] <>
                            maybe [ ] viewingStyle (notebook ^. _viewingCell))
             $ catMaybes [ if not (notebook ^. _editable) then Nothing else Just $ header c
                         , if hidden then Nothing else Just $ editor notebook c
                         , Just $ statusBar notebook (isJust outputContent) c
                         , outputContent
                         ]
     ]
  where
  hidden = (c ^. _hiddenEditor) || (not (notebook ^. _editable))

  viewingStyle cellId =
    if cellId == c ^. _cellId
    then [ ]
    else [ CSS.style (display displayNone) ]

header :: forall e. Cell -> HTML e
header c =
  let info = cellInfo c
  in H.div [ A.classes [VC.cellHeader, B.clearfix] ]
           [ H.div [ A.class_ VC.cellIcon ]
                   [ glyph info.glyph ]
           , H.div [ A.class_ VC.cellName ]
                   [ H.text info.name ]
           , controls c
           ]

controls :: forall e. Cell -> HTML e
controls c =
  H.div [ A.classes [B.pullRight, VC.cellControls] ]
        [ H.button [ A.title if c ^. _hiddenEditor
                             then "Show cell options"
                             else "Hide cell options"
                   , E.onClick $ E.input_ $ UpdateCell (c ^. _cellId) (_hiddenEditor %~ not) ]
                   [ glyph if c ^. _hiddenEditor
                           then B.glyphiconEyeOpen
                           else B.glyphiconEyeClose
                   ]
        , H.button [ A.title "Delete cell"
                   , E.onClick $ E.input_ $ TrashCell $ c ^. _cellId ]
                   [ glyph B.glyphiconTrash ]
        , glyph B.glyphiconChevronLeft
        ]

output :: forall e. State -> Cell -> Maybe (HTML e)
output notebook c =
  let out = renderOutput (notebook ^. _browserFeatures) c
  in if null out
     then Nothing
     else Just $ H.div [ A.classes [B.row, VC.cellOutput] ]
                       [ H.div [ A.class_ VC.cellOutputLabel ]
                               $ cellOutputBar notebook c
                       , H.div [ A.class_ VC.cellOutputResult ]
                               out
                       ]

cellOutputBar :: forall e. State -> Cell -> Array (HTML e)
cellOutputBar notebook c =
  case c ^. _output of
    PortResource res ->
      [ H.text $ resourceName res ++ " := "
      , H.ul [ A.classes [VC.nextCellList] ]
             [ li "Query this output" (E.input_ $ InsertCell c $ newQueryContent res) B.glyphiconHdd
             , li "Search this output" (E.input_ $ InsertCell c $ newSearchContent res) B.glyphiconSearch
             , li "Visualize this output" (\_ -> pure $ insertViz notebook c) B.glyphiconPicture
             ]
      ]

    VarMap _ ->
      [ H.ul [ A.classes [VC.nextCellList] ]
             [ li "Query using fields" (E.input_ $ InsertCell c $ Query Qu.initialQueryRec) B.glyphiconHdd
             ]
      ]

    _ -> []
  where
  li :: String -> _ -> A.ClassName -> HTML e
  li title handler c =
    H.li_ [ H.button [A.title title, A.ariaLabel title, E.onClick handler] [glyph c] ]

statusBar :: forall e. State -> Boolean -> Cell -> HTML e
statusBar notebook hasOutput c =
  row' (fadeWhen (c ^. _hiddenEditor))
  $ if (c ^. _hiddenEditor)
    then []
    else [ H.div [ A.classes [VC.cellEvalLine, B.clearfix] ]
           $ [ H.button [ A.classes [B.btn, B.btnPrimary, buttonClass]
                        , E.onClick \_ -> pure $ handleRunClick (notebook ^._notebook) c
                        , A.ariaLabel buttonAriaLabel
                        ]
               [ glyph buttonGlyph ]
             , H.div [ A.classes [ VC.statusText ] ]
               [ H.text $ statusText notebook.tickDate (c ^. _runState) ]
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
  buttonClass = if isRunning c then VC.stopButton else VC.playButton

  buttonGlyph :: A.ClassName
  buttonGlyph = if isRunning c then B.glyphiconStop else B.glyphiconPlay

  buttonAriaLabel :: String
  buttonAriaLabel = if isRunning c then "Stop" else "Play"

  messages :: Array (HTML e)
  messages = message c

  toggleMessageButton :: Maybe (HTML e)
  toggleMessageButton =
    if null messages
    then Nothing
    else Just $ H.button [ A.title if c ^._expandedStatus
                                   then "Hide messages"
                                   else "Show messages"
                         , E.onClick (\_ -> E.preventDefault $> pure (UpdateCell (c ^._cellId) (_expandedStatus %~ not))) ]
         [ glyph if c ^._expandedStatus
                 then B.glyphiconEyeClose
                 else B.glyphiconEyeOpen ]

  linkButton :: Maybe (HTML e)
  linkButton =
    if not hasOutput
    then Nothing
    else Just $ H.button [ A.title "Embed cell output"
                         , E.onClick (\_ -> pure $ handleEmbedClick notebook c)
                         , E.onMouseEnter $ E.input_ (UpdateCell (c ^. _cellId) (_embedHovered .~ true))
                         , E.onMouseLeave $ E.input_ (UpdateCell (c ^. _cellId) (_embedHovered .~ false))
                         ]
         [ H.img [ A.src if (c ^. _embedHovered)
                         then "img/code-icon-blue.svg"
                         else "img/code-icon.svg"
                 , A.width 16.0 ] [] ]

  refreshButton :: Maybe (HTML e)
  refreshButton =
    Just $ H.button [ A.title "Refresh cell content"
                    , A.classes [VC.refreshButton]
                    , E.onClick (\_ -> pure $ handleRefreshClick c)
                    ]
    [ glyph B.glyphiconRefresh ]

message :: forall e. Cell -> Array (HTML e)
message c =
  let collapsed = if c ^._expandedStatus then [] else [VC.collapsed]
  in if null (c ^._failures)
     then if (c ^._message) /= ""
          then [ H.div [ A.classes $ [VC.cellMessages] ++ collapsed ]
                       $ if c ^._expandedStatus
                         then [ H.div_ []
                              , messageText c
                              ]
                         else []
               ]
          else [ ]
     else [ H.div [ A.classes $ [VC.cellFailures] ++ collapsed ]
                              $ failureText c
          ]

details :: forall e. Cell -> Array (HTML e)
details c = commonMessage "" [H.div_ [H.text (c^._message)]] c

messageText :: forall e. Cell -> HTML e
messageText c =
  if isJust $ indexOf "\n" m
  then H.pre_ [H.text m]
  else H.div_ [H.text m]
  where m = c ^._message

failureText :: forall e. Cell -> Array (HTML e)
failureText c =
  commonMessage
  (show (length fs) <> " error(s) during evaluation. ")
  ((\f -> if isJust $ indexOf "\n" f
          then H.pre_ [H.text f]
          else H.div_ [H.text f]) <$> fs)
  c
  where fs = c ^._failures

commonMessage :: forall e. String -> Array (HTML e) -> Cell -> Array (HTML e)
commonMessage intro children c =
  [ H.div_ [ H.text intro ]
  ] <>
  if (c ^._expandedStatus)
  then children
  else [ ]

statusText :: Maybe Date -> RunState -> String
statusText _ RunInitial = ""
statusText d (RunningSince d') =
  maybe "" (\s -> "Running for " <> s <> "s") $ ((flip secondsText d') <$> d)
statusText _ (RunFinished (Milliseconds ms)) =
  "Finished: took "
  <> (maybe "0" show $ fromNumber $ Math.floor ms)
  <> "ms."

secondsText :: Date -> Date -> String
secondsText a b = maybe "0" show <<< fromNumber <<< Math.floor <<< max zero <<<
                  unSeconds $ on (-) (toSeconds <<< toEpochMilliseconds) a b
  where
  max :: forall a. (Ord a) => a -> a -> a
  max a b = if a > b then a else b
  unSeconds (Seconds n) = n

cellInfo :: Cell -> { name :: String, glyph :: A.ClassName }
cellInfo c = case c ^. _content of
  Explore _ -> { name: "Explore", glyph: B.glyphiconEyeOpen }
  Markdown _ -> { name: "Markdown", glyph: B.glyphiconEdit }
  Search _ -> { name: "Search", glyph: B.glyphiconSearch }
  Visualize _ -> { name: "Visualize", glyph: B.glyphiconPicture }
  Query _ -> { name: "Query", glyph: B.glyphiconHdd }

editor :: forall e. State -> Cell -> HTML e
editor state c = case c ^. _content of
  Explore rec -> exploreEditor c
  Search _ -> searchEditor (state ^._notebook) c
  Visualize _ -> vizChoices c
  _ -> aceEditor c

renderOutput :: forall e. BrowserFeatures -> Cell -> Array (HTML e)
renderOutput bf c =
  if not (null $ c ^. _failures)
  then []
  else case c ^. _content of
    Explore _ -> exploreOutput c
    Markdown mr -> markdownOutput bf mr c
    Search _ -> searchOutput c
    Visualize _ -> vizOutput c
    Query _ -> queryOutput c
