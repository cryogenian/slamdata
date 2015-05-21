module View.Notebook.Cell.Markdown (markdownOutput) where

import Model.Notebook.Cell (CellId())
import View.Notebook.Common (HTML())
import qualified View.Css as VC
import View.Common (fadeWhen)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events.Monad as E
import Text.Markdown.SlamDown.Html (SlamDownEvent(), renderHalogen)
import Text.Markdown.SlamDown.Parser (parseMd)
import EffectTypes (NotebookAppEff()) 
import Input.Notebook (Input(..))

markdownOutput :: forall e. String -> CellId -> HTML e
markdownOutput s cellId =
  H.div [A.classes ([ VC.markdownOutput ] <> (fadeWhen $ s == "")) ]
  <<< fromSlamDownEvents <<< renderHalogen $ parseMd s
  where
  fromSlamDownEvents :: [H.HTML (E.Event (NotebookAppEff e) SlamDownEvent)] -> [HTML e]
  fromSlamDownEvents = ((((CellSlamDownEvent cellId) <$>) <$>) <$>)
