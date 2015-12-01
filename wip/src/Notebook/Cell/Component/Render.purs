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

module Notebook.Cell.Component.Render
  ( CellHTML()
  , header
  , statusBar
  ) where

import Prelude

import Data.Array (catMaybes, null)
import Data.Int (fromNumber)
import Data.Lens ((^?))
import Data.Maybe (Maybe(..), maybe)
import Data.Time (Seconds(..), Milliseconds(..), toSeconds)

import Halogen (ParentHTML())
import Halogen.HTML.Events as E
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (glyph, glyphInactive)
import Render.CssClasses as CSS

import Notebook.Cell.RunState (RunState(..), isRunning)
import Notebook.Cell.Component.Query (CellQuery(..), InnerCellQuery())
import Notebook.Cell.Component.State (CellState(), AnyCellState(), _cachingEnabled)
import Notebook.Common (Slam())

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
        [ P.title if cs.isCollapsed then "Show cell options" else "Hide cell options"
        , E.onClick (E.input_ ToggleCollapsed)
        ]
        [ glyph if cs.isCollapsed then B.glyphiconEyeOpen else B.glyphiconEyeClose ]
    , H.button
        [ P.title "Delete cell"
        , E.onClick (E.input_ TrashCell)
        ]
        [ glyph B.glyphiconTrash ]
    , glyph B.glyphiconChevronLeft
    ]

statusBar :: Boolean -> CellState -> CellHTML
statusBar hasResults cs =
  H.div
    [ P.classes [CSS.cellEvalLine, B.clearfix, B.row] ]
    $ [ H.button
          [ P.classes [B.btn, B.btnPrimary, button.className]
          , E.onClick (E.input_ RunCell)
          -- , P.ariaLabel button.label
          ]
          [ glyph button.glyph ]
      , H.div
          [ P.class_ CSS.statusText ]
          [ H.text $ runStatusMessage cs.runState ]
      , H.div
          [ P.classes [B.pullRight, CSS.cellControls] ]
          $ catMaybes
              [ Just refreshButton
              , toggleCachingButton cs
              , toggleMessageButton cs
              , if hasResults then Just linkButton else Nothing
              , Just $ glyph B.glyphiconChevronLeft
              ]
      ]
     <> messages cs
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
    , P.classes [CSS.refreshButton]
    , E.onClick (E.input_ RefreshCell)
    ]
    [ glyph B.glyphiconRefresh ]

toggleMessageButton :: CellState -> Maybe CellHTML
toggleMessageButton cs =
  if null cs.messages
  then Nothing
  else Just $
    H.button
      [ P.title if cs.isCollapsed then "Show messages" else "Hide messages"
      , E.onClick (E.input_ ToggleMessages)
      ]
      [ glyph if cs.isCollapsed then B.glyphiconEyeOpen else B.glyphiconEyeClose ]

toggleCachingButton :: CellState -> Maybe CellHTML
toggleCachingButton cs =
  (cs ^? _cachingEnabled) <#> \cachingEnabled ->
    H.button
      [ P.title if cachingEnabled then "Disable Caching" else "Enable Caching"
      , E.onClick (E.input_ ToggleCaching)
      ]
      [ B.glyphiconPushpin # if cachingEnabled then glyph else glyphInactive
      ]

linkButton :: CellHTML
linkButton =
  H.button
    [ P.title "Embed cell output"
    , E.onClick (E.input_ ShareCell)
    ]
    [ H.span [ P.class_ CSS.shareButton ] [] ]

messages :: CellState -> Array (CellHTML)
messages cs = []
  -- let numErrors = length (isLeft `filter` cs.messages)
--   if null cs.messages
--   then []
--   else
--     [ H.div
--         [ P.classes $ [CSS.cellMessages] ++ if cs.isCollapsed then [CSS.collapsed] else [] ]
--         if cs.isCollapsed
--         then []
--         else [ H.div_ [] ] ++ message `map` cs.messages

-- message :: Either String String -> CellHTML
-- message (Left msg) = failureText msg
-- message (Right msg) = messageText msg
--     ]
--   let collapsed =
--   in
--      then case cs.message of
--       Nothing -> []
--       Just message' ->

--      else
--       [ H.div
--           [ P.classes $ [CSS.cellFailures] ++ collapsed ]
--           $ failureText cs
--       ]

-- messageText :: String -> CellHTML
-- messageText m =
--   let tag = if isJust $ indexOf "\n" m then H.pre_ else H.div_
--   in tag [H.text m]

-- failureText :: CellState -> Array (CellHTML)
-- failureText cs =
--   [ H.div_ [H.text $ show (length cs.failures) <> " error(s) during evaluation. "] ]
--   <> if not cs.showMessages
--      then []
--      else messageText <$> cs.failures

