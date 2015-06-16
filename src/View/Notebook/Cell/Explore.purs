module View.Notebook.Cell.Explore (exploreEditor, exploreOutput) where

import Controller.Notebook.Cell.Explore (runExplore)
import Model.Notebook.Cell (Cell(), _FileInput, _JTableContent, _content, _input, _output)
import Optic.Core ((..), (.~))
import View.Notebook.Cell.FileInput (fileInput)
import View.Notebook.Cell.JTableCell (renderJTableOutput)
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified View.Css as VC

exploreEditor :: forall e. Cell -> HTML e
exploreEditor cell = H.div [ A.class_ VC.exploreCellEditor ] $
                     fileInput (_content .. _FileInput) (\port -> (_input .~ port) .. (_output .~ port)) cell


exploreOutput :: forall e. Cell -> [HTML e]
exploreOutput = renderJTableOutput (_content .. _JTableContent) runExplore
