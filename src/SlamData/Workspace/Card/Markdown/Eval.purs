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

module SlamData.Workspace.Card.Markdown.Eval
  ( evalMarkdown
  , evalMarkdownForm
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Array as A
import Data.Identity (Identity)
import Data.Json.Extended as EJSON
import Data.List as L
import Data.String as S
import Data.StrMap as SM
import Matryoshka (project, transAna)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as Quasar
import SlamData.SqlSquared.Tagged as SqlT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Markdown.Component.State as MDS
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Port as Port
import SqlSquared as Sql
import Text.Parsing.Parser (parseErrorMessage)
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Eval as SDE
import Text.Markdown.SlamDown.Halogen.Component.State as SDH
import Text.Markdown.SlamDown.Parser as SDP
import Text.Markdown.SlamDown.Traverse as SDT
import Utils.Path (DirPath)

evalMarkdownForm
  ∷ ∀ m
  . MonadEff SlamDataEffects m
  ⇒ Monad m
  ⇒ MD.Model
  → SD.SlamDownP Port.VarMapValue
  → Port.DataMap
  → m Port.Out
evalMarkdownForm model doc varMap = do
  let inputState = SDH.formStateFromDocument doc
  thisVarMap ← liftEff $ MDS.formStateToVarMap inputState model
  pure (Port.Variables × map Right thisVarMap `SM.union` varMap)

evalMarkdown
  ∷ ∀ m
  . MonadEff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ String
  → Port.DataMap
  → m Port.Out
evalMarkdown str varMap = do
  CEM.CardEnv { path } ← ask
  case SDP.parseMd str of
    Left e → CE.throwMarkdownError (CE.MarkdownParseError {markdown: str, error: e})
    Right sd → do
      let sm = map (Sql.print ∘ unwrap) $ Port.flattenResources varMap
      doc ← evalEmbeddedQueries sm path sd
      pure (Port.SlamDown doc × varMap)

findFields
  ∷ ∀ a
  . SD.SlamDownP a
  → Array String
findFields = SDT.everything (const mempty) extractField
  where
  extractField ∷ SD.Inline a → Array String
  extractField (SD.FormField label _ _) = pure label
  extractField _ = mempty

evalEmbeddedQueries
  ∷ ∀ m
  . MonadEff SlamDataEffects m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ SM.StrMap String
  → DirPath
  → SD.SlamDownP Port.VarMapValue
  → m (SD.SlamDownP Port.VarMapValue)
evalEmbeddedQueries sm dir =
  SDE.eval
    { code: evalCode
    , textBox: evalTextBox
    , value: evalValue
    , list: evalList
    }

  where

  evalCode
    ∷ Maybe SDE.LanguageId
    → String
    → m Port.VarMapValue
  evalCode mid code
    | languageIsSql mid =
        Port.VarMapValue ∘ transAna Sql.Literal ∘ extractCodeValue <$> runQuery code
    | otherwise =
        pure $ Port.VarMapValue $ Sql.null

  extractCodeValue ∷ Array EJSON.EJson → EJSON.EJson
  extractCodeValue [ej] = extractSingletonObject ej
  extractCodeValue arr = EJSON.array arr

  languageIsSql
    ∷ Maybe SDE.LanguageId
    → Boolean
  languageIsSql =
    maybe true $ eq "sql" ∘ S.toLower

  extractSingletonObject
    ∷ EJSON.EJson
    → EJSON.EJson
  extractSingletonObject lit =
    case project lit of
      EJSON.Map (EJSON.EJsonMap [key × val]) → val
      _ → lit

  evalValue
    ∷ String
    → m Port.VarMapValue
  evalValue code = do
    maybe
      (Port.VarMapValue Sql.null)
      (Port.VarMapValue ∘ transAna Sql.Literal ∘ extractSingletonObject)
      ∘ A.head
      <$> runQuery code

  evalTextBox
    ∷ SD.TextBox (Const String)
    → m (SD.TextBox Identity)
  evalTextBox tb = do
    let sql = unwrap $ SD.traverseTextBox (map \_ → Const unit) tb
    mresult ← A.head <$> runQuery sql
    result ←
      maybe (CE.throwMarkdownError CE.MarkdownNoTextBoxResults) (pure ∘ extractSingletonObject) mresult
    case tb, project result of
      (SD.PlainText _), (EJSON.String str) →
        pure ∘ SD.PlainText $ pure str
      (SD.Numeric _), (EJSON.Decimal a) →
        pure ∘ SD.Numeric $ pure a
      (SD.Time prec _), (EJSON.String time) →
        case SqlT.parseTime time of
          Left error → CE.throwMarkdownError (CE.MarkdownInvalidTimeValue { time, error })
          Right r → pure ∘ SD.Time prec $ pure r
      (SD.Date _), (EJSON.String date) →
        case SqlT.parseDate date of
          Left error → CE.throwMarkdownError (CE.MarkdownInvalidDateValue { date, error })
          Right r → pure ∘ SD.Date $ pure r
      (SD.DateTime prec _), (EJSON.String datetime) →
        case SqlT.parseDateTime datetime of
          Left error → CE.throwMarkdownError (CE.MarkdownInvalidDateTimeValue { datetime, error })
          Right r → pure ∘ SD.DateTime prec $ pure r
      _, _ → CE.throwMarkdownError (CE.MarkdownTypeError (show result) (show tb))

  evalList
    ∷ String
    → m (L.List Port.VarMapValue)
  evalList code = do
    jitems ← map extractSingletonObject <$> runQuery code
    let limit = 500
    pure ∘ L.fromFoldable
      $ if A.length jitems > limit
          then
            ( map (Port.VarMapValue ∘ transAna Sql.Literal) $ A.take limit jitems )
            ⊕ [ SD.stringValue $ "<" ⊕ show limit ⊕ "item limit reached>" ]
          else
          map (Port.VarMapValue ∘ transAna Sql.Literal) jitems

  runQuery
    ∷ String
    → m (Array EJSON.EJson)
  runQuery code =
    let esql = Sql.parse code
    in case esql of
      Left error →
        CE.throwMarkdownError (CE.MarkdownSqlParseError { sql: code, error: parseErrorMessage error })
      Right sql → do
        {inputs} ← CE.liftQ $ Quasar.compile dir sql sm
        CEM.addSources inputs
        CE.liftQ $ Quasar.queryEJsonVM dir sql sm
