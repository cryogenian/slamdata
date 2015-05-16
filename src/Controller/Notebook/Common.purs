module Controller.Notebook.Common where

import EffectTypes (NotebookAppEff())
import Halogen.HTML.Events.Monad (Event())
import Input.Notebook (Input())

type I e = Event (NotebookAppEff e) Input
