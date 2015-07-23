module Driver.Notebook.Cell where

import Prelude
import Control.Monad.Eff (Eff())
import Controller.Notebook.Cell (runCell, viewCell)
import Controller.Notebook.Common (I())
import Data.Date (now)
import EffectTypes (NotebookAppEff(), NotebookComponentEff())
import Halogen (Driver())
import Halogen.HTML.Events.Monad (runEvent)
import Input.Notebook (Input(..))
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _content)
import Optic.Getter ((^.))


driveEvent :: forall e. Driver Input (NotebookComponentEff e) -> I e -> Eff (NotebookAppEff e) Unit
driveEvent = runEvent (const $ return unit)

driveCellContent :: forall eff. Input -> Driver Input (NotebookComponentEff eff) -> Eff (NotebookAppEff eff) Unit
driveCellContent (RequestCellContent cell) driver = 
  case cell ^. _content of
    Explore _ -> runWith cell driver
    Search _ -> runWith cell driver  
    Visualize _ -> runWith cell driver
    _ -> return unit
driveCellContent (ReceiveCellContent cell) driver =
  case cell ^. _content of
    Query _ -> runWith cell driver
    Markdown _ -> runWith cell driver
    _ -> pure unit

driveCellContent (ViewCellContent cell) driver =
  case cell ^. _content of
    Search _ -> view cell driver
    Explore _ -> view cell driver
    Visualize _ -> view cell driver
    Query _ -> view cell driver
    _ -> pure unit 

driveCellContent _ _ = pure unit

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
