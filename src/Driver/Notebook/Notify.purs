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
import Data.Foldable (traverse_, fold)
import Data.Map (toList, empty, insert, lookup, Map(), delete)
import Data.Maybe (maybe, Maybe(..), isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid.All (All(..), runAll)
import Data.Monoid.First (First(..), runFirst)
import Data.Tuple (fst, snd)
import EffectTypes
import Halogen (Driver())
import Input.Notebook (Input(..))
import Model.Notebook (State(), _notebook, _requesting)
import Model.Notebook.Cell (Cell(), _cellId, CellId())
import Model.Notebook.Domain
import Optic.Core  ((^.), (%~))
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
      -- after we get requested id we can safely remove it from
      -- notebook state 
      driver $ WithState (_requesting %~ filter (/= cellId))
    modifyRef nKnot (insert cellId t) 
  go state cellId = notify (state ^. _notebook) cellId (state ^. _requesting) driver 

    


notify :: forall e. Notebook -> CellId -> [CellId] -> 
          Driver Input (NotebookComponentEff e) ->
          Eff (NotebookAppEff e) Unit
notify notebook cellId requestedIds driver =
  -- If current cell is parent for all requested cell
  -- and we can get next cell in ancestors list
  if isParent && isJust nextCell
  -- then we get this next cell
  then request $ fromJust nextCell
  -- else we get all children
  else requestForChildren
  where
  request :: Cell -> Eff _ Unit
  request = driver <<< RequestCellContent

  requestForChildren :: Eff _ Unit 
  requestForChildren = traverse_ request $ dependentCells cellId

  -- list of list of dependencies of requested cell from top to bottom
  ancestors' :: [[CellId]]
  ancestors' = flip ancestors (notebook ^._dependencies) <$> requestedIds

  -- if current cell is parent for all requested cells
  isParent :: Boolean
  isParent = runAll $ fold (All <$> (elem cellId) <$> ancestors')

  -- if current cell is parent for all requested cells we get
  -- next cell in this hierarchy and return it 
  nextCell :: Maybe Cell
  nextCell = do
    cid <- runFirst $ fold
           (First <$> ((\as -> do
                           i <- case elemIndex cellId as of
                             -1 -> Nothing
                             i -> pure i
                           as !! (i + 1)) <$> ancestors'))

    notebook ^? cellById cid 

  dependentCells :: CellId -> [Cell]
  dependentCells cid = filter (\x -> elem (x ^._cellId) (dependencies cid))
                       (notebook ^. _cells)

  dependencies :: CellId -> [CellId]
  dependencies = flip descendants (notebook ^._dependencies)

