{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Controller.Notebook.Cell.Search (
  runSearch
  , viewSearch
  ) where

import Prelude
import Control.Plus (empty)
import Control.Apply (lift2)
import Control.Bind ((>=>))
import Control.MonadPlus (guard)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class (liftAff)
import Optic.Core
import Optic.Fold ((^?))

import Data.Maybe (fromMaybe, maybe, Maybe(..))
import Data.Either (either, Either(..))
import Data.Either.Unsafe (fromRight)
import Data.Tuple (Tuple(..), uncurry)
import Data.StrMap (keys)
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
import Api.Query (fields, sample, templated)

runSearch :: forall eff. Cell -> I eff
runSearch cell =
  either (const errorInParse) go $ mkQuery $ toLower buffer
  where
  input :: Maybe Resource
  input = cell ^? _input .. _PortResource

  output :: Maybe Resource
  output = cell ^? _output .. _PortResource

  buffer :: String
  buffer = cell ^. _content .. _Search .. _buffer

  go :: _ -> I eff
  go q = do
    case input of
      Nothing -> errorInFields
      Just inp -> do
        efs <- liftAff $ attempt $ fields inp
        case efs of
          Left _ -> errorInFields
          Right fs -> do
            let tmpl = queryToSQL fs q
                sql :: Maybe String
                sql = templated <$> input <*> (pure tmpl)
            case sql of
              Nothing -> errorInParse
              Just s ->
                (update cell (_message .~ ("Generated SQL: " <> s)))
                `andThen` \_ ->
                (fromMaybe errorInPorts (queryToJTable cell tmpl <$> input <*> output))
  errorInParse :: I eff
  errorInParse =
    update cell (_failures .~ ["Incorrect query string"])
      `andThen` \_ -> finish cell

  errorInFields :: I eff
  errorInFields =
    update cell (_failures .~ ["selected file is empty"])
      `andThen` \_ -> finish cell

  errorInPorts :: I eff
  errorInPorts =
    update cell (_failures .~ ["Incorrect type of input or output"])
      `andThen` \_ -> finish cell


viewSearch :: forall e. Cell -> I e
viewSearch cell =
  maybe error (flip runJTable cell) (cell ^? _output.._PortResource)
  where
  error :: I e
  error =
    update cell (_failures .~ ["Incorrect type of input"])
      `andThen` \_ -> finish cell

