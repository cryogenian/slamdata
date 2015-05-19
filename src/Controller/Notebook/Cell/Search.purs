module Controller.Notebook.Cell.Search (
  runSearch 
  ) where

import Control.Plus (empty)
import Control.Apply (lift2)
import Control.Bind ((>=>))
import Control.MonadPlus (guard)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class (liftAff)
import Optic.Core ((^.), (.~), (..))
import Optic.Fold ((^?))

import Data.Maybe (fromMaybe, maybe, Maybe(..))
import Data.Either (either, Either())
import Data.Either.Unsafe (fromRight)
import Data.Tuple (Tuple(..), uncurry)
import Data.StrMap (keys)
import Data.Int (Int())
import Data.Time (Milliseconds())
import Data.Date (now, toEpochMilliseconds, nowEpochMilliseconds)

import Data.Argonaut.Core (JObject(), Json(), JArray(), fromObject)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Decode (decodeJson)
import Halogen.HTML.Events.Monad (andThen)
import Text.SlamSearch (mkQuery)

import Input.Notebook (Input(..))
import Controller.Notebook.Common (I())
import Controller.Notebook.Cell.JTableContent (runJTable, queryToJTable)
import Model.Resource (newFile, _path, AnyPath(), Resource())
import Model.Notebook.Port (_PortResource)
import Model.Notebook.Search (needFields, queryToSQL)
import Model.Notebook.Cell (Cell(), RunState(..), _RunningSince, _runState,  _cellId, _content, _Search, _failures, _input, _output)
import Model.Notebook.Cell.Search (SearchRec(), _buffer, initialSearchRec) 
import Api.Fs (delete)
import Api.Query (fields, port, sample)

runSearch :: forall eff. Cell -> I eff
runSearch cell =
  either errorInParse go $ mkQuery buffer
  where
  input :: Maybe Resource
  input = cell ^? _input .. _PortResource
  
  output :: Maybe Resource
  output = cell ^? _output .. _PortResource

  buffer :: String
  buffer = cell ^. _content .. _Search .. _buffer

  update :: (Cell -> Cell) -> Input 
  update = UpdateCell (cell ^. _cellId)

  started :: Maybe Milliseconds
  started = toEpochMilliseconds <$> (cell ^? _runState .. _RunningSince)

  go :: _ -> I eff
  go q = do
    fs <- maybe (pure $ pure []) (liftAff <<< attempt <<< fields) do 
      guard (needFields q)
      input
    flip (either errorInFields) fs \fs ->
      fromMaybe errorInPorts
      (queryToJTable cell (queryToSQL fs q) <$> input <*> output)

  errorInParse :: _ -> I eff
  errorInParse _ =
    (pure $ update (_failures .~ ["Incorrect query string"]))
    `andThen` \_ -> finish

  errorInFields :: _ -> I eff
  errorInFields _ =
    (pure $ update (_failures .~ ["selected file is empty"]))
    `andThen` \_ -> finish

  errorInPorts :: I eff
  errorInPorts =
    (pure $ update (_failures .~ ["Incorrect type of input or output"]))
    `andThen` \_ -> finish 

  finish :: I eff
  finish = do 
    d <- liftEff nowEpochMilliseconds
    pure $ update (_runState .~ RunFinished (maybe zero (d -) started))
