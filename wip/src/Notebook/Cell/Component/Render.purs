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

module Notebook.Cell.Component.Render (CellHTML(), header, statusBar) where

import Prelude

import Data.Array (catMaybes, null, length)
import Data.Maybe (Maybe(..), isJust)
import Data.String (indexOf)

import Halogen (ParentHTML())
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (fadeWhen, glyph, row')
import Render.CssClasses as CSS

import Notebook.Cell.Component.Query (CellQuery(..), InnerCellQuery())
import Notebook.Cell.Component.State (CellState(), AnyCellState(), isRunning)
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
          [ P.classes [B.btn, B.btnPrimary, buttonClass]
          , E.onClick (E.input_ RunCell)
          -- , P.ariaLabel buttonAriaLabel
          ]
          [ glyph buttonGlyph ]
      , H.div
          [ P.class_ CSS.statusText ]
          [ H.text "" ] -- $ statusText notebook.tickDate (c ^. _runState) ]
      , H.div
          [ P.classes [B.pullRight, CSS.cellControls] ]
          $ catMaybes
              [ Just refreshButton
              , toggleMessageButton cs
              , if hasResults then Just linkButton else Nothing
              , Just $ glyph B.glyphiconChevronLeft
              ]
      ]
     <> messages cs
  where
  buttonClass :: H.ClassName
  buttonClass = if isRunning cs then CSS.stopButton else CSS.playButton

  buttonGlyph :: H.ClassName
  buttonGlyph = if isRunning cs then B.glyphiconStop else B.glyphiconPlay

  buttonAriaLabel :: String
  buttonAriaLabel = if isRunning cs then "Stop" else "Play"

refreshButton :: CellHTML
refreshButton =
  H.button
    [ P.title "Refresh cell content"
    , P.classes [CSS.refreshButton]
    , E.onClick (E.input_ RefreshCell)
    ]
    [ glyph B.glyphiconRefresh ]

toggleMessageButton :: CellState -> Maybe (CellHTML)
toggleMessageButton cs =
  case cs.messages of
    [] -> Nothing
    _ -> Just $
      H.button
        [ P.title if cs.isCollapsed then "Show messages" else "Hide messages"
        , E.onClick (E.input_ ToggleMessages)
        ]
        [ glyph if cs.isCollapsed then B.glyphiconEyeOpen else B.glyphiconEyeClose ]

linkButton :: CellHTML
linkButton =
  H.button
    [ P.title "Embed cell output"
    , E.onClick (E.input_ ShareCell)
    ]
    [ H.img
        [ P.src "img/code-icon.svg"
          -- TODO: not this! ew
          -- if (c ^. _embedHovered) then "img/code-icon-blue.svg" else "img/code-icon.svg"
        , P.width (P.Pixels 16.0)
        ]
    ]

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

