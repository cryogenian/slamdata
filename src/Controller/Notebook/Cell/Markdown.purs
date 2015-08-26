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

module Controller.Notebook.Cell.Markdown (runMarkdown) where

import Prelude
import Api.Query (query')
import Control.Bind ((<=<), (=<<))
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error(), error, message)
import Control.Monad.State.Class (modify, get)
import Control.Monad.State.Trans (StateT(), evalStateT)
import Control.Monad.Error.Class (throwError)
import Controller.Notebook.Common (I())
import Data.Argonaut.Core (Json(), JArray(), JObject(), toObject, foldJson)
import Data.Bifunctor (bimap)
import Data.Date (now)
import Data.Either (Either(..), either)
import Data.List (List(), (:), fromList, toList, head)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (mempty)
import Data.Path.Pathy ((</>), file)
import Data.String (joinWith, toLower)
import EffectTypes (NotebookAppEff())
import Input.Notebook (Input(..), CellResultContent(MarkdownContent))
import Model.Notebook.Cell (Cell(), FailureMessage(), _cellId, _pathToNotebook, _content, _Markdown)
import Model.Notebook.Cell.Markdown (_input)
import Model.Resource (Resource(..))
import Optic.Core
import Optic.Fold ((^?))
import Text.Markdown.SlamDown (SlamDown(..), TextBoxType(..), eval)
import Text.Markdown.SlamDown.Parser (parseMd)
import qualified Data.Array as A
import qualified Data.Array.NonEmpty as NEL
import qualified Data.StrMap as SM

type QM eff = StateT Int (Aff (NotebookAppEff eff))

runMarkdown :: forall eff. Cell -> I eff
runMarkdown cell = do
  let input = cell ^? _content .. _Markdown .. _input
  e <- liftAff $ attempt $ flip evalStateT 0 $ maybe (pure $ SlamDown mempty) (eval evalFuncs <<< parseMd) input
  d <- liftEff now
  pure $ CellResult (cell ^. _cellId) d $ bimap handleErr MarkdownContent e

  where

  handleErr :: Error -> NEL.NonEmpty FailureMessage
  handleErr err = NEL.singleton ("An error occurred in a embedded query: " ++ message err)

  tmpFile :: Int -> Resource
  tmpFile n = File $ cell ^. _pathToNotebook </> file ("tmp" <> show (cell ^._cellId) <> "-" <> show n)

  runQuery :: String -> QM eff JArray
  runQuery code = do
    n <- get :: _ Int
    modify (+ 1)
    result <- liftAff $ query' (tmpFile n) code
    either (throwError <<< error) pure result

  evalFuncs = { code: evalCode, block: evalBlock
              , text: evalText
              , value: evalValue, list: evalList
              }

  evalCode :: String -> QM eff String
  evalCode = evalValue

  evalBlock :: String -> List String -> QM eff String
  evalBlock ty code =
    let code' = joinWith "\n" (fromList code)
    in if toLower ty == "sql" || ty == "" then evalValue code' else pure code'

  evalText :: TextBoxType -> String -> QM eff String
  evalText _ = evalValue

  evalValue :: String -> QM eff String
  evalValue = pure <<< fromMaybe "" <<< (fromRow <=< A.head) <=< runQuery

  fromRow :: Json -> Maybe String
  fromRow = fromValue <=< head <<< SM.values <=< toObject

  fromValue :: Json -> Maybe String
  fromValue = foldJson (const Nothing) (Just <<< show) (Just <<< show) Just (const Nothing) (const Nothing)

  evalList :: String -> QM eff (List String)
  evalList code = do
    items <- A.mapMaybe fromRow <$> runQuery code
    if A.length items > 500
      then pure $ toList (A.take 500 items ++ ["<500 item limit reached>"])
      else pure $ toList items

