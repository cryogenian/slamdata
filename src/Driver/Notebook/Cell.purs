module Driver.Notebook.Cell where

import Control.Monad.Eff (Eff())
import Controller.Notebook.Cell (runCell, viewCell)
import Controller.Notebook.Common (I())
import Data.Date (now)
import EffectTypes (NotebookAppEff(), NotebookComponentEff())
import Halogen (Driver())
import Halogen.HTML.Events.Monad (runEvent)
import Input.Notebook (Input(..))
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _content)
import Optic.Core ((^.))

driveEvent :: forall e. Driver Input (NotebookComponentEff e) -> I e -> Eff (NotebookAppEff e) Unit
driveEvent = runEvent (const $ return unit)

driveCellContent :: forall eff. Input -> Driver Input (NotebookComponentEff eff) -> Eff (NotebookAppEff eff) Unit
driveCellContent (RequestCellContent cell) driver =
  case cell ^. _content of
    Search _ -> runWith cell driver
    Explore _ -> runWith cell driver
    Visualize _ -> runWith cell driver
    _ -> return unit
driveCellContent (ReceiveCellContent cell) driver = runWith cell driver
driveCellContent (ViewCellContent cell) driver =
  case cell ^. _content of
    Search _ -> view cell driver
    Explore _ -> view cell driver
    Visualize _ -> view cell driver
    Query _ -> view cell driver
    _ -> pure unit 
driveCellContent _ _ = return unit

runWith :: forall eff. Cell -> Driver Input (NotebookComponentEff eff) -> Eff (NotebookAppEff eff) Unit
runWith cell driver = do
  d <- now
  driver $ StartRunCell (cell ^. _cellId) d
  driveEvent driver $ runCell cell

view :: forall eff. Cell -> Driver Input (NotebookComponentEff eff) -> Eff (NotebookAppEff eff) Unit
view cell driver = do
  d <- now
  driver $ StartRunCell (cell ^. _cellId) d
  driveEvent driver $ viewCell cell

