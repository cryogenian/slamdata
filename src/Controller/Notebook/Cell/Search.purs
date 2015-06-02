module Controller.Notebook.Cell.Search (
  runSearch
  , viewSearch
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
import Data.String (toLower)

import Data.Argonaut.Core (JObject(), Json(), JArray(), fromObject)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Decode (decodeJson)
import Halogen.HTML.Events.Monad (andThen)
import Text.SlamSearch (mkQuery)

import Input.Notebook (Input(..))
import Controller.Notebook.Common (I(), finish, update)
import Controller.Notebook.Cell.JTableContent (runJTable, queryToJTable)
import Model.Path (AnyPath())
import Model.Resource (newFile, _path, Resource())
import Model.Notebook.Port (_PortResource)
import Model.Notebook.Search (needFields, queryToSQL)
import Model.Notebook.Cell (Cell(), RunState(..), _RunningSince, _runState,  _cellId, _content, _Search, _failures, _input, _output, _message)
import Model.Notebook.Cell.Search (SearchRec(), _buffer, initialSearchRec)
import Api.Fs (delete)
import Api.Query (fields, port, sample, templated)

runSearch :: forall eff. Cell -> I eff
runSearch cell =
  either errorInParse go $ mkQuery $ toLower buffer
  where
  input :: Maybe Resource
  input = cell ^? _input .. _PortResource

  output :: Maybe Resource
  output = cell ^? _output .. _PortResource

  buffer :: String
  buffer = cell ^. _content .. _Search .. _buffer

  go :: _ -> I eff
  go q = do
    fs <- maybe (pure $ pure []) (liftAff <<< attempt <<< fields) do
      guard (needFields q)
      input
    flip (either errorInFields) fs \fs ->
      let tmpl = queryToSQL fs q
          sql :: Maybe String
          sql = templated <$> input <*> (pure tmpl) in
      maybe empty (\s -> update cell (_message .~ ("Generated SQL: " <> s))) sql
      `andThen` \_ ->
      (fromMaybe errorInPorts (queryToJTable cell tmpl <$> input <*> output))



  errorInParse :: _ -> I eff
  errorInParse _ =
    update cell (_failures .~ ["Incorrect query string"])
      `andThen` \_ -> finish cell

  errorInFields :: _ -> I eff
  errorInFields _ =
    update cell (_failures .~ ["selected file is empty"])
      `andThen` \_ -> finish cell

  errorInPorts :: I eff
  errorInPorts =
    update cell (_failures .~ ["Incorrect type of input or output"])
      `andThen` \_ -> finish cell


viewSearch :: forall e. Cell -> I e
viewSearch cell =
  maybe error (flip runJTable cell) (cell ^? _input.._PortResource)
  where
  error :: I e
  error =
    update cell (_failures .~ ["Incorrect type of input"])
      `andThen` \_ -> finish cell
  
