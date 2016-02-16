{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook.Cell.Component.Render
  ( CellHTML()
  , header
  , statusBar
  ) where

import Prelude

import Data.Array as A
import Data.Either (either, isLeft)
import Data.Int (fromNumber)
import Data.Lens ((^?))
import Data.Maybe (Maybe(..), maybe)
import Data.Time (Seconds(..), Milliseconds(..), toSeconds)
import Data.Visibility (Visibility(..))

import Halogen (ParentHTML())
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Notebook.Cell.Component.Query (CellQuery(..), InnerCellQuery())
import SlamData.Notebook.Cell.Component.State (CellState(), AnyCellState(), _cachingEnabled)
import SlamData.Notebook.Cell.RunState (RunState(..), isRunning)
import SlamData.Effects (Slam())
import SlamData.Render.Common (glyph, glyphInactive)
import SlamData.Render.CSS as CSS

type CellHTML = ParentHTML AnyCellState CellQuery InnerCellQuery Slam Unit

header :: forall r. { name :: String, glyph :: H.ClassName | r } -> CellState -> CellHTML
header def cs =
  H.div
    [ P.classes [CSS.cellHeader, B.clearfix] ]
    [ H.div
        [ P.class_ CSS.cellIcon ]
        [ glyph def.glyph ]
    , H.div
        [ P.class_ CSS.cellName ]
        [ H.text def.name ]
    , controls cs
    ]

controls :: CellState -> CellHTML
controls cs =
  H.div
    [ P.classes [B.pullRight, CSS.cellControls] ]
    [ H.button
        [ P.title cellOptionsLabel
        , ARIA.label cellOptionsLabel
        , E.onClick (E.input_ ToggleCollapsed)
        ]
        [ glyph if cs.isCollapsed then B.glyphiconEyeOpen else B.glyphiconEyeClose ]
    , H.button
        [ P.title "Delete cell"
        , ARIA.label "Delete cell"
        , E.onClick (E.input_ TrashCell)
        ]
        [ glyph B.glyphiconTrash ]
    , glyph B.glyphiconChevronLeft
    ]
  where
  cellOptionsLabel = if cs.isCollapsed then "Show cell options" else "Hide cell options"

statusBar :: Boolean -> CellState -> CellHTML
statusBar hasResults cs =
  H.div
    [ P.classes [CSS.cellEvalLine, B.clearfix, B.row] ]
    $ [ H.button
          [ P.classes [B.btn, B.btnPrimary, button.className]
          , E.onClick (E.input_ $ if isRunning cs.runState
                                  then StopCell
                                  else RunCell)
          , P.title button.label
          , ARIA.label button.label
          ]
          [ glyph button.glyph ]
      , H.div
          [ P.class_ CSS.statusText ]
          [ H.text $ runStatusMessage cs.runState ]
      , H.div
          [ P.classes [B.pullRight, CSS.cellControls] ]
          $ A.catMaybes
              [ toggleMessageButton cs
              , toggleCachingButton cs
              , if hasResults then Just refreshButton else Nothing
              , if hasResults then Just linkButton else Nothing
              , Just $ glyph B.glyphiconChevronLeft
              ]
      ]
     <> statusMessages cs
  where

  button =
    if isRunning cs.runState
    then { className: CSS.stopButton, glyph: B.glyphiconStop, label: "Stop" }
    else { className: CSS.playButton, glyph: B.glyphiconPlay, label: "Play" }

runStatusMessage :: RunState -> String
runStatusMessage RunInitial = ""
runStatusMessage (RunElapsed t) =
  "Running for " <> printSeconds t <> "..."
runStatusMessage (RunFinished t) =
  "Finished: took " <> printMilliseconds t <> "."

printSeconds :: Milliseconds -> String
printSeconds t = case toSeconds t of
  Seconds s -> maybe "0" show (fromNumber (Math.floor s)) ++ "s"

printMilliseconds :: Milliseconds -> String
printMilliseconds (Milliseconds ms) = maybe "0" show (fromNumber (Math.floor ms)) ++ "ms"

refreshButton :: CellHTML
refreshButton =
  H.button
    [ P.title "Refresh cell content"
    , ARIA.label "Refresh cell content"
    , P.classes [CSS.refreshButton]
    , E.onClick (E.input_ RefreshCell)
    ]
    [ glyph B.glyphiconRefresh ]

toggleMessageButton :: CellState -> Maybe CellHTML
toggleMessageButton { messages, messageVisibility } =
  if A.null messages
  then Nothing
  else Just $
    H.button
      [ P.title label
      , ARIA.label label
      , E.onClick (E.input_ ToggleMessages)
      ]
      [ glyph if isCollapsed then B.glyphiconEyeOpen else B.glyphiconEyeClose ]
  where
  label = if isCollapsed then "Show messages" else "Hide messages"
  isCollapsed = messageVisibility == Invisible

toggleCachingButton :: CellState -> Maybe CellHTML
toggleCachingButton cs =
  (cs ^? _cachingEnabled) <#> \cachingEnabled ->
    H.button
      [ P.title $ label cachingEnabled
      , ARIA.label $ label cachingEnabled
      , E.onClick (E.input_ ToggleCaching)
      ]
      [ B.glyphiconPushpin # if cachingEnabled then glyph else glyphInactive
      ]
  where
  label cachingEnabled = if cachingEnabled then "Disable Caching" else "Enable Caching"

linkButton :: CellHTML
linkButton =
  H.button
    [ P.title "Embed cell output"
    , ARIA.label "Embed cell output"
    , E.onClick (E.input_ ShareCell)
    ]
    [ H.span [ P.class_ CSS.shareButton ] [] ]

statusMessages :: CellState -> Array (CellHTML)
statusMessages { messages, messageVisibility }
  | A.null messages = []
  | otherwise =
      [ H.div
          [ P.classes classes ]
           $ [ H.div_ failureMessage ]
          ++ if isCollapsed
             then []
             else map (either message message) messages'
      ]
  where
  isCollapsed = messageVisibility == Invisible
  errorMessages = let es = A.filter isLeft messages in es
  hasErrors = not (A.null errorMessages)
  messages' = if hasErrors then errorMessages else messages
  classes =
    let cls = if hasErrors then CSS.cellFailures else CSS.cellMessages
    in if isCollapsed then [cls, CSS.collapsed] else [cls]
  failureMessage =
    if hasErrors
    then
      let numErrors = A.length errorMessages
          s = if numErrors == 1 then "" else "s"
      in [H.text $ show numErrors <> " error" <> s <> " during evaluation. "]
    else []

message :: String -> CellHTML
message = H.pre_ <<< pure <<< H.text
