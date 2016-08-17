{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Quasar.Query
  ( SQL
  , templated
  , compile
  , query
  , queryEJson
  , queryEJsonVM
  , viewQuery
  , fileQuery
  , all
  , sample
  , count
  , fields
  , module Quasar.Error
  ) where

import SlamData.Prelude

import Control.Apply (lift2)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Argonaut as JS
import Data.Array as Arr
import Data.Int as Int
import Data.Json.Extended as EJS
import Data.Path.Pathy as P
import Data.String as S
import Data.StrMap as SM

import Quasar.Advanced.QuasarAF as QF
import Quasar.Data (JSONMode(..))
import Quasar.Error (QError)
import Quasar.Mount as QM
import Quasar.Types (AnyPath, DirPath, FilePath, CompileResultR)

import SlamData.Quasar.Aff (QEff, runQuasarF, Wiring)
import SlamData.Quasar.Error (throw)

-- | This is template string where actual path is encoded like {{path}}
type SQL = String

-- | Replaces `{{path}}` placeholders in an SQL template string with a file
-- | path.
templated ∷ FilePath → SQL → SQL
templated res = S.replace "{{path}}" ("`" <> P.printPath res <> "`")

-- | Compiles a query.
-- |
-- | If a file path is provided for the input path the query can use the
-- | {{path}} template syntax to have the file's path inserted, and the file's
-- | parent directory will be used to determine the backend to use in Quasar.
compile
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → AnyPath
  → SQL
  → SM.StrMap String
  → m (Either QError CompileResultR)
compile wiring path sql varMap =
  let
    backendPath = either id (fromMaybe P.rootDir <<< P.parentDir) path
    sql' = maybe sql (flip templated sql) $ either (const Nothing) Just path
  in
    runQuasarF wiring $ QF.compileQuery backendPath sql' varMap

query
  ∷ ∀ r eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → SQL
  → m (Either QError JS.JArray)
query wiring path sql =
  runQuasarF wiring $ QF.readQuery Readable path sql SM.empty Nothing

queryEJson
  ∷ ∀ r eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → SQL
  → m (Either QError (Array EJS.EJson))
queryEJson wiring path sql =
  runQuasarF wiring $ QF.readQueryEJson path sql SM.empty Nothing

queryEJsonVM
  ∷ ∀ r eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → SQL
  → SM.StrMap String
  → m (Either QError (Array EJS.EJson))
queryEJsonVM wiring path sql vm =
  runQuasarF wiring $ QF.readQueryEJson path sql vm Nothing

-- | Runs a query creating a view mount for the query.
-- |
-- | If a file path is provided for the input path the query can use the
-- | {{path}} template syntax to have the file's path inserted.
viewQuery
  ∷ ∀ r eff m
  . (Affable (QEff eff) m, Monad m)
  ⇒ Wiring r
  → AnyPath
  → FilePath
  → SQL
  → SM.StrMap String
  → m (Either QError Unit)
viewQuery wiring path dest sql vars = do
  runQuasarF wiring $
    QF.deleteMount (Right dest)
  runQuasarF wiring $
    QF.updateMount (Right dest) (QM.ViewConfig
      { query: maybe sql (flip templated sql) $ either (const Nothing) Just path
      , vars
      })

-- | Runs a query for a particular file (the query can use the {{path}} template
-- | syntax to have the file's path inserted), writing the results to a file.
-- | The query backend will be determined by the input file path.
-- |
-- | The returned value is the output path returned by Quasar. For some queries
-- | this will be the input file rather than the specified destination.
fileQuery
  ∷ ∀ r eff m
  . Affable (QEff eff) m
  ⇒ Wiring r
  → FilePath
  → FilePath
  → SQL
  → SM.StrMap String
  → m (Either QError FilePath)
fileQuery wiring file dest sql vars =
  let backendPath = fromMaybe P.rootDir (P.parentDir file)
  in runQuasarF wiring $ map _.out <$>
    QF.writeQuery backendPath dest (templated file sql) vars

all
  ∷ ∀ r eff m
  . Affable (QEff eff) m
  ⇒ Wiring r
  → FilePath
  → m (Either QError JS.JArray)
all wiring file =
  runQuasarF wiring $ QF.readFile Readable file Nothing

sample
  ∷ ∀ r eff m
  . Affable (QEff eff) m
  ⇒ Wiring r
  → FilePath
  → Int
  → Int
  → m (Either QError JS.JArray)
sample wiring file offset limit =
  runQuasarF wiring $ QF.readFile Readable file (Just { limit, offset })

count
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → FilePath
  → m (Either QError Int)
count wiring file = runExceptT do
  let backendPath = fromMaybe P.rootDir (P.parentDir file)
      sql = templated file "SELECT COUNT(*) as total FROM {{path}}"
  result ← ExceptT $ runQuasarF wiring $
    QF.readQuery Readable backendPath sql SM.empty Nothing
  pure $ fromMaybe 0 (readTotal result)
  where
  readTotal ∷ JS.JArray → Maybe Int
  readTotal =
    Int.fromNumber
      <=< JS.toNumber
      <=< SM.lookup "total"
      <=< JS.toObject
      <=< Arr.head

fields
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → FilePath
  → m (Either QError (Array String))
fields wiring file = runExceptT do
  jarr ← ExceptT $ sample wiring file 0 100
  case jarr of
    [] → throw "empty file"
    _ → pure $ Arr.nub $ getFields =<< jarr

  where
  -- The output of this function is mysterious, but luckily is used in just one place.
  --
  -- TODO: Rather than accumulating a an array of formatted strings, this should be refactored
  -- to return an array of *arrays* of unformatted strings, which can then be formatted by the
  -- client (e.g. to intercalate with dots and add backticks).
  getFields ∷ JS.Json → Array String
  getFields = Arr.filter (_ /= "") <<< Arr.nub <<< go []
    where
    go ∷ Array String → JS.Json → Array String
    go [] json = go [""] json
    go acc json =
      if JS.isObject json
      then maybe acc (goObj acc) $ JS.toObject json
      else if JS.isArray json
           then maybe acc (goArr acc) $ JS.toArray json
           else acc

      where
      goArr ∷ Array String → JS.JArray → Array String
      goArr acc arr =
        Arr.concat $ go (lift2 append acc $ mkArrIxs arr) <$> arr
        where
        mkArrIxs ∷ JS.JArray → Array String
        mkArrIxs jarr =
          map (\x → "[" <> show x <> "]") $ Arr.range 0 $ Arr.length jarr - 1

      goObj ∷ Array String → JS.JObject → Array String
      goObj acc = Arr.concat <<< map (goTuple acc) <<< Arr.fromFoldable <<< SM.toList

      goTuple ∷ Array String → Tuple String JS.Json → Array String
      goTuple acc (Tuple key json) =
        go ((\x → x <> ".`" <> key <> "`") <$> acc) json
