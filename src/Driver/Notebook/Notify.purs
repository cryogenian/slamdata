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
import Data.Foldable (traverse_)
import Data.Map (toList, empty, insert, lookup, Map(), delete)
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


type NotifyKnot = Map CellId Timeout 

notifyDriver :: forall e. RefVal State -> RefVal NotifyKnot -> Input ->
                Driver Input (NotebookComponentEff e) -> 
                Eff (NotebookAppEff e) Unit
notifyDriver sKnot nKnot input driver =
  case input of
    -- We've got result from other cell, just notify children
    CellResult cellId _ (Right _) -> do 
      state <- readRef sKnot
      go state cellId
    -- Markdown cell's updated, check timeout and notify children
    CellSlamDownEvent cellId _ -> do
      state <- readRef sKnot
      map <- readRef nKnot
      maybe
        (setTimeout state cellId)
        (\t -> do
            clearTimeout t
            setTimeout state cellId)
        (lookup cellId map) 
    _ -> pure unit
  where
  setTimeout state cellId = do
    t <- timeout Config.notifyTimeout do
      go state cellId
      modifyRef nKnot (delete cellId) 
    modifyRef nKnot (insert cellId t) 
  go state cellId = 
    maybe (pure unit)
    (notify (state ^. _notebook) cellId driver)
    (state ^._requesting)
    


notify :: forall e. Notebook -> CellId -> 
          Driver Input (NotebookComponentEff e) -> CellId -> 
          Eff (NotebookAppEff e) Unit
notify notebook cellId driver requestedId =
  -- check if current cell is parent of requested 
  case elemIndex cellId ancestors' of
    -- It's not, update children of this cell
    -1 -> traverse_ request $ dependentCells cellId
    -- It is. Get next cell in this hierarchy and if there is
    -- no such cell update children for requested cell. 
    x -> maybe (traverse_ request $ dependentCells requestedId) request
         ((ancestors' <> [requestedId]) !! (x + 1) >>=
          \x -> notebook ^? cellById x) 
  where
  request :: Cell -> Eff _ Unit
  request = driver <<< RequestCellContent

  -- list of dependencies of requested cell from top to bottom
  ancestors' :: [CellId]
  ancestors' = ancestors requestedId (notebook ^._dependencies)

  dependentCells :: CellId -> [Cell]
  dependentCells cid = filter (\x -> elem (x ^._cellId) (dependencies cid))
                       (notebook ^. _cells)

  dependencies :: CellId -> [CellId]
  dependencies = flip descendants (notebook ^._dependencies)

