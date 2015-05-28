module Driver.Notebook.Cell where

import Control.Monad.Eff (Eff())
import Controller.Notebook.Cell (runCell)
import Controller.Notebook.Common (I())
import Data.Date (now)
import EffectTypes (NotebookAppEff(), NotebookComponentEff())
import Halogen (Driver())
import Halogen.HTML.Events.Monad (runEvent)
import Input.Notebook (Input(..))
import Model.Notebook.Cell (_cellId)
import Optic.Core ((^.))

driveEvent :: forall e. Driver Input (NotebookComponentEff e) -> I e -> Eff (NotebookAppEff e) Unit
driveEvent = runEvent (const $ return unit)

driveCellContent :: forall eff. Input -> Driver Input (NotebookComponentEff eff) -> Eff (NotebookAppEff eff) Unit
driveCellContent (ReceiveCellContent cell) driver = do
  d <- now
  driver $ StartRunCell (cell ^. _cellId) d
  driveEvent driver $ runCell cell
driveCellContent _ _ = return unit
