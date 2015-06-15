module Driver.Notebook.Notify where 

import qualified Config as Config 
import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (RefVal(), readRef, modifyRef, writeRef)
import Control.Timer (Timeout(), timeout, clearTimeout)
import Controller.Notebook.Cell (requestCellContent)
import Controller.Notebook.Common (I())
import Data.Array (filter, elemIndex, (!!), singleton)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (toList)
import Data.Maybe (maybe, Maybe(..))
import Data.Tuple (fst, snd)
import EffectTypes
import Halogen (Driver())
import Input.Notebook (Input(..))
import Model.Notebook (State(), _notebook, _requesting)
import Model.Notebook.Cell (Cell(), _cellId, CellId())
import Model.Notebook.Domain
import Optic.Core  ((^.))
import Optic.Fold ((^?))
import Utils (elem)


type NotifyKnot = Maybe Timeout 

notifyDriver :: forall e. RefVal State -> RefVal NotifyKnot -> Input ->
                Driver Input (NotebookComponentEff e) -> 
                Eff (NotebookAppEff e) Unit
notifyDriver sKnot nKnot input driver = do
  state <- readRef sKnot
  maybe
    (pure unit)
    (notifyChildren (state ^._notebook) nKnot input driver)
    (state ^. _requesting)

notifyChildren :: forall e. Notebook -> 
                  RefVal NotifyKnot -> Input -> 
                  Driver Input (NotebookComponentEff e) ->
                  CellId ->
                  Eff (NotebookAppEff e) Unit
notifyChildren notebook nKnot (CellResult cellId _ (Right _)) driver requestedId =
  notify notebook requestedId cellId nKnot driver 
notifyChildren notebook nKnot (CellSlamDownEvent cellId _) driver requestedId =
  notify notebook requestedId cellId nKnot driver
notifyChildren _ _ _ _ _ = pure unit

notify :: forall e. Notebook -> CellId -> CellId -> RefVal NotifyKnot -> 
          Driver Input (NotebookComponentEff e) ->
          Eff (NotebookAppEff e) Unit
notify notebook requestedId cellId nKnot driver = do
  mbTimeout <- readRef nKnot
  maybe setTimeout (\t -> clearTimeout t *> setTimeout) mbTimeout
  where
  setTimeout = do
    t <- timeout Config.notifyTimeout go
    writeRef nKnot (Just t) 
    
  go = case elemIndex cellId ancestors of
    -1 -> for_ (dependentCells cellId) request
    x -> maybe (for_ ds request) request 
        ((ancestors <> [requestedId]) !! (x + 1) >>=
         \x -> notebook ^? cellById x)

  request = driver <<< RequestCellContent

  ancestors = deps requestedId (notebook ^._dependencies)
  
  ds = dependentCells requestedId

  dependentCells cid = filter (\x -> elem (x ^._cellId) (dependencies cid))
                       (notebook ^. _cells)

  dependencies cid = fst <$> (filter (\x -> snd x == cid) $
                              toList (notebook ^._dependencies))

