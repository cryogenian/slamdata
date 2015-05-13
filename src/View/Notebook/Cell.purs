module View.Notebook.Cell (cell) where

import Control.Functor (($>))
import Control.Plus (empty)
import Data.Array (length)
import Data.Date (Date(), toEpochMilliseconds)
import Data.Function (on)
import Data.Inject1 (inj)
import Data.Maybe (Maybe(), maybe)
import Data.Time (Milliseconds(..), Seconds(..), toSeconds)
import EffectTypes (NotebookAppEff())
import Input.Notebook (Input(..), runCellEvent)
import Model.Notebook.Cell
import Number.Format (toFixed)
import Optic.Core ((^.))
import Text.Markdown.SlamDown.Html (SlamDownEvent(), renderHalogen)
import Text.Markdown.SlamDown.Parser (parseMd)
import View.Common
import View.Notebook.Cell.Explore (exploreEditor, exploreOutput)
import View.Notebook.Cell.Search (searchOutput)
import View.Notebook.Common
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC

cell :: forall e. Maybe Date -> Cell -> [HTML e]
cell d state =
  [ H.div [ A.classes [B.containerFluid, VC.notebookCell] ]
          [ row [ H.div [ A.classes [B.btnGroup, B.pullRight, VC.cellControls] ]
                        [ H.button [ A.classes [B.btn]
                                   , E.onClick $ E.input_ $ ToggleEditorCell $ state ^. _cellId
                                   ]
                                   [ H.text (if state ^. _hiddenEditor then "Show" else "Hide") ]
                        , H.button [ A.classes [B.btn]
                                   , E.onClick $ E.input_ $ TrashCell $ state ^. _cellId
                                   ]
                                   [ H.text "Trash" ]
                        ]
                ]
          , editor state
          , row [ H.div [ A.classes $ fadeWhen $ state ^. _hiddenEditor ]
                        [ H.button [ A.classes [ B.btn, B.btnPrimary, if isRunning (state ^. _runState) then VC.stopButton else VC.playButton ]
                                   , E.onClick (\_ -> pure (runCellEvent (state ^. _cellId)))
                                   ]
                                   [ if isRunning (state ^. _runState) then glyph B.glyphiconStop else glyph B.glyphiconPlay ]
                        ]
                , H.div [ A.classes [ VC.statusText ] ]
                        [ H.text $ statusText d (state ^. _runState) ]
                , H.div [ A.classes [ VC.cellFailures ] ]
                        (failureText (state ^. _cellId) (state ^. _expandedStatus) (state ^. _failures))
                ]
          , H.div [ A.classes [B.row, VC.cellOutput] ]
                  $ renderOutput (state ^. _cellType) (state ^. _content)
          , H.div [ A.classes [B.row, VC.cellNextActions] ]
                  []
          ]
  ]

failureText :: forall e. CellId -> Boolean -> [FailureMessage] -> [HTML e]
failureText _ _ [ ] = [ ]
failureText cellId expanded fs =
  [ H.div_ [ H.text (show (length fs) <> " error(s) during evaluation. ")
  , H.a [ A.href "#", E.onClick (\_ -> E.preventDefault $> pure (ToggleFailuresCell cellId)) ] linkText ]
  ] <>
    if expanded
    then (\f -> H.div_ [ H.text f ]) <$> fs
    else [ ]
  where linkText =
          [ H.text (if expanded
                    then "Hide details"
                    else "Show details") ]

isRunning :: RunState -> Boolean
isRunning (RunningSince _) = true
isRunning _ = false

statusText :: Maybe Date -> RunState -> String
statusText _ RunInitial = ""
statusText d (RunningSince d') = maybe "" (\s -> "Running for " <> s <> "s") $ d >>= flip secondsText d'
statusText _ (RunFinished (Milliseconds ms)) = "Finished: took " <> show ms <> "ms"

secondsText :: Date -> Date -> Maybe String
secondsText a b = toFixed 0 <<< Math.max 0 <<< unSeconds $ on (-) (toSeconds <<< toEpochMilliseconds) a b
  where unSeconds (Seconds n) = n

editor :: forall e. Cell -> HTML e
editor state = case state ^. _cellType of
  Explore -> exploreEditor
  _ -> row [ H.div [ A.classes $ [VC.cellInput] <> fadeWhen (state ^. _hiddenEditor) ]
                      [ H.div [ dataCellId $ state ^. _cellId
                              , dataCellType $ state ^. _cellType
                              , A.classes [ VC.aceContainer ]
                              ]
                              []
                      ]
              ]
  where
  dataCellId :: forall i. Number -> A.Attr i
  dataCellId = A.attr (A.attributeName "data-cell-id")

  dataCellType :: forall i. CellType -> A.Attr i
  dataCellType = A.attr (A.attributeName "data-cell-type") <<< celltype2str

renderOutput :: forall e. CellType -> String -> [HTML e]
renderOutput Explore = exploreOutput
renderOutput Markdown = markdownOutput
renderOutput Search = searchOutput
renderOutput _ = const [ ]

-- TODO: Interpret the SlamDownEvent instead of discarding.
markdownOutput :: forall e. String -> [HTML e]
markdownOutput = fromSlamDownEvents <<< renderHalogen <<< parseMd
  where fromSlamDownEvents :: [H.HTML (E.Event (NotebookAppEff e) SlamDownEvent)] -> [HTML e]
        fromSlamDownEvents = (($> empty) <$>)

fadeWhen :: Boolean -> [A.ClassName]
fadeWhen true = [B.fade]
fadeWhen false = [B.fade, B.in_]
