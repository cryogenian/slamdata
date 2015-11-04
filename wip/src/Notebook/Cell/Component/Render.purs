module Notebook.Cell.Component.Render (container) where

import Prelude

import Control.Bind (join)

import Data.Array (catMaybes, null, length)
import Data.Maybe (Maybe(..), isJust)
import Data.String (indexOf)

import Halogen (ParentHTML())
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (fadeWhen, glyph, row, row')
import Render.CssClasses as CSS

import Notebook.Cell.Component.Def (Def(), CellPart())
import Notebook.Cell.Component.Query (CellQuery(..), InnerCellQuery())
import Notebook.Cell.Component.State (CellState(), AnyCellState(), isRunning)
import Notebook.Common (Slam())

type CellHTML = ParentHTML AnyCellState CellQuery InnerCellQuery Slam CellPart

container
  :: forall se fe sr fr
   . Def se fe sr fr
  -> CellState
  -> Maybe CellHTML
  -> CellHTML
  -> CellHTML
container def cs editor results =
  H.div
    [ P.classes $ join [containerClasses, collapsedClass] ]
    $ catMaybes
        [ if cs.isNotebookEditable then Just $ header def cs else Nothing
        , row <<< pure <$> editor
        , Just $ statusBar cs.hasResults cs
        , Just (row [results])
        ]
  where

  containerClasses = [B.containerFluid, CSS.notebookCell, B.clearfix]
  collapsedClass = if cs.showEditor then [CSS.collapsed] else []

header :: forall se fe sr fr. Def se fe sr fr -> CellState -> CellHTML
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
        [ P.title if cs.showEditor then "Hide cell options" else "Show cell options"
        , E.onClick (E.input_ ToggleEditor)
        ]
        [ glyph if cs.showEditor then B.glyphiconEyeClose else B.glyphiconEyeOpen ]
    , H.button
        [ P.title "Delete cell"
        , E.onClick (E.input_ TrashCell)
        ]
        [ glyph B.glyphiconTrash ]
    , glyph B.glyphiconChevronLeft
    ]

statusBar :: Boolean -> CellState -> CellHTML
statusBar hasResults cs =
  row' (fadeWhen $ not cs.showEditor)
  $ if (not cs.showEditor)
    then []
    else pure $
      H.div
        [ P.classes [CSS.cellEvalLine, B.clearfix] ]
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
  case cs.message of
    Nothing -> Nothing
    Just msg -> Just $
      H.button
        [ P.title if cs.showMessages then "Hide messages" else "Show messages"
        , E.onClick (E.input_ ToggleMessages)
        ]
        [ glyph if cs.showMessages then B.glyphiconEyeClose else B.glyphiconEyeOpen ]

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
messages cs =
  let collapsed = if cs.showMessages then [] else [CSS.collapsed]
  in if null cs.failures
     then case cs.message of
      Nothing -> []
      Just message' ->
        [ H.div
            [ P.classes $ [CSS.cellMessages] ++ collapsed ]
            if cs.showMessages then [ H.div_ [], messageText message' ] else []
        ]
     else
      [ H.div
          [ P.classes $ [CSS.cellFailures] ++ collapsed ]
          $ failureText cs
      ]

messageText :: String -> CellHTML
messageText m =
  let tag = if isJust $ indexOf "\n" m then H.pre_ else H.div_
  in tag [H.text m]

failureText :: CellState -> Array (CellHTML)
failureText cs =
  [ H.div_ [H.text $ show (length cs.failures) <> " error(s) during evaluation. "] ]
  <> if not cs.showMessages
     then []
     else messageText <$> cs.failures

