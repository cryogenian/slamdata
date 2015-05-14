module View.Notebook.Cell.Search where

import Control.Plus
import Data.Either 
import Data.Maybe
import Data.String.Regex (regex, noFlags, replace)
import Data.Inject1 (inj)
import View.Common 
import View.Notebook.Common
import Model.Notebook.Cell
import EffectTypes 
import Input.Notebook (Input(..), NotebookInput(..))


import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E 
import qualified Halogen.HTML.Events.Forms as E


import Optic.Core ((^.), (.~), (..))


handleSearchInput :: forall e. Cell -> String -> E.Event (NotebookAppEff e) Input
handleSearchInput cell value = do
  pure $ inj $ UpdateCell (cell ^. _cellId) (_content .. _search .~ value)

  
searchEditor :: forall e. Cell -> HTML e
searchEditor cell =
  row [ H.form_
        [H.input 
         [ A.value (cell ^. _content .. _search)
         , E.onInput (\v -> pure $ handleSearchInput cell v) 
         ] [ ] ] ]

searchOutput :: forall e i. String -> [ H.HTML (E.Event e i) ]
searchOutput rawInput =
  [ H.p_ [ H.text $ "trololo" ] ]
--  let input = replace (regex "\\s+" noFlags{global = true}) " " rawInput in
--  flip (either (const [])) (mkQuery input) $ \query -> 
--  [ H.p_ [ H.text $ "select * from path where " <> (queryToSQL query) ] ]

