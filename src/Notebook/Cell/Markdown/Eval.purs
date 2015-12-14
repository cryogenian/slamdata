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

module Notebook.Cell.Markdown.Eval (markdownEval) where

import Prelude

import Control.Bind ((<=<))
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (StateT(), evalStateT, get, modify)

import Data.Argonaut.Core (Json())
import Data.Argonaut.Core as JSON
import Data.Array as A
import Data.Either (Either(..), either)
import Data.List (List(), fromList, toList, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Path.Pathy ((</>), file)
import Data.String as Str
import Data.StrMap as SM

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Parser as SD

import Model.CellId (CellId(), cellIdToString)
import Model.Port (Port(..))
import Model.Resource (Resource(..))

import Notebook.Cell.Common.EvalQuery (CellEvalResult(), CellEvalInput())
import Notebook.Common (Slam())

import Quasar.Aff as Quasar

import Utils.Path (DirPath())

markdownEval :: CellEvalInput -> String -> Slam CellEvalResult
markdownEval { cellId, notebookPath } s = do
  result <- attempt $ evalEmbeddedQueries notebookPath cellId (SD.parseMd s)
  pure case result of
    Left err ->
      { messages: [ Left (show err) ]
      , output: Nothing
      }
    Right doc ->
      { messages: [ Right $ "Exported fields: " ++ Str.joinWith ", " (findFields doc) ]
      , output: Just (SlamDown doc)
      }

findFields :: SD.SlamDown -> Array String
findFields = SD.everything (const mempty) extractField
  where
  extractField :: SD.Inline -> Array String
  extractField (SD.FormField label _ _) = pure label
  extractField _ = mempty

type EvalM = StateT Int Slam

evalEmbeddedQueries :: Maybe DirPath -> CellId -> SD.SlamDown -> Slam SD.SlamDown
evalEmbeddedQueries dir cellId =
  flip evalStateT 0
    <<< SD.eval
      { code: evalValue
      , block: evalBlock
      , text: evalText
      , value: evalValue
      , list: evalList
      }

  where

  evalBlock :: String -> List String -> EvalM String
  evalBlock ty code =
    let code' = Str.joinWith "\n" (fromList code)
    in if Str.toLower ty == "sql" || ty == "" then evalValue code' else pure code'

  evalText :: SD.TextBoxType -> String -> EvalM String
  evalText _ = evalValue

  evalValue :: String -> EvalM String
  evalValue = pure <<< fromMaybe "" <<< (fromRow <=< A.head) <=< runQuery

  evalList :: String -> EvalM (List String)
  evalList code = do
    items <- A.mapMaybe fromRow <$> runQuery code
    if A.length items > 500
      then pure $ toList (A.take 500 items ++ ["<500 item limit reached>"])
      else pure $ toList items

  fromRow :: Json -> Maybe String
  fromRow = fromValue <=< head <<< SM.values <=< JSON.toObject

  fromValue :: Json -> Maybe String
  fromValue =
    JSON.foldJson
      (const Nothing)
      (Just <<< show)
      (Just <<< show)
      Just
      (const Nothing)
      (const Nothing)

  runQuery :: String -> EvalM JSON.JArray
  runQuery code = case dir of
    Nothing -> throwError (error "Cannot evaluate markdown without a saved notebook path")
    Just dir' -> do
      n <- get :: EvalM Int
      modify (+ 1)
      let tempFile = File $ dir' </> file ("tmp" <> cellIdToString cellId <> "-" <> show n)
      result <- liftAff $ Quasar.query' tempFile code
      either (throwError <<< error) pure result
