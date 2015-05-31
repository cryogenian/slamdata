module View.Notebook.Cell.Markdown (markdownOutput) where

import Data.Array (null)
import EffectTypes (NotebookAppEff())
import Input.Notebook (Input(..))
import Model.Notebook.Cell (CellId())
import Text.Markdown.SlamDown.Html (SlamDownEvent(), renderHalogen)
import Text.Markdown.SlamDown.Parser (parseMd)
import View.Common (fadeWhen)
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events.Monad as E
import qualified View.Css as VC

markdownOutput :: forall e. String -> CellId -> [HTML e]
markdownOutput s cellId =
  let md = renderHalogen $ parseMd s
  in if null md
     then []
     else [ H.div [ A.classes ([ VC.markdownOutput ] <> (fadeWhen $ s == "")) ]
                  $ fromSlamDownEvents $ md
          ]
  where
  fromSlamDownEvents :: [H.HTML (E.Event (NotebookAppEff e) SlamDownEvent)] -> [HTML e]
  fromSlamDownEvents = ((((CellSlamDownEvent cellId) <$>) <$>) <$>)
