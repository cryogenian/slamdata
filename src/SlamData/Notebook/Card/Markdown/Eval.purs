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

module SlamData.Notebook.Card.Markdown.Eval
  ( markdownEval
  , markdownSetup
  ) where

import SlamData.Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Aff.Free as AffF
import Control.Monad.Eff.Class as Eff
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as Err
import Control.Monad.State.Trans as State

import Data.Argonaut.Core as JSON
import Data.Array as A
import Data.Date as D
import Data.Date.Locale as DL
import Data.Enum as Enum
import Data.Foldable as F
import Data.Functor.Mu as Mu
import Data.Identity (Identity)
import Data.List as L
import Data.NaturalTransformation as NT
import Data.SQL2.Literal as SQL2
import Data.String as S
import Data.StrMap as SM
import Data.Time as DT

import SlamData.Effects (Slam)
import SlamData.Notebook.Card.Ace.Component as ACE
import SlamData.Notebook.Card.CardId as CID
import SlamData.Notebook.Card.Common.EvalQuery as CEQ
import SlamData.Notebook.Card.Port as Port
import SlamData.Quasar.Query as Quasar

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Traverse as SDT
import Text.Markdown.SlamDown.Eval as SDE
import Text.Markdown.SlamDown.Parser as SDP

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

import Utils.Path (DirPath)

markdownEval
  ∷ CEQ.CardEvalInput
  → String
  → ACE.AceDSL CEQ.CardEvalResult
markdownEval { cardId, notebookPath } str =
  AffF.fromAff do
    result ← Aff.attempt ∘ evalEmbeddedQueries notebookPath cardId $ SDP.parseMd str
    pure
      case result of
        Left err →
          { messages: [ Left $ Exn.message err ]
          , output: Nothing
          }
        Right doc →
          { messages: [ Right $ "Exported fields: " ⊕ S.joinWith ", " (findFields doc) ]
          , output: Just $ Port.SlamDown doc
          }

markdownSetup
  ∷ CEQ.CardSetupInfo
  → ACE.AceDSL Unit
markdownSetup _ =
  pure unit

findFields
  ∷ ∀ a
  . SD.SlamDownP a
  → Array String
findFields = SDT.everything (const mempty) extractField
  where
  extractField ∷ SD.Inline a → Array String
  extractField (SD.FormField label _ _) = pure label
  extractField _ = mempty

type EvalM = State.StateT Int Slam

freshInt ∷ EvalM Int
freshInt = do
  n ← State.get ∷ EvalM Int
  State.modify (_ + 1)
  pure n

runEvalM ∷ NT.Natural EvalM Slam
runEvalM = flip State.evalStateT 0

evalEmbeddedQueries
  ∷ Maybe DirPath
  → CID.CardId
  → SD.SlamDownP Port.VarMapValue
  → Slam (SD.SlamDownP Port.VarMapValue)
evalEmbeddedQueries dir cardId =
  runEvalM ∘
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
    → EvalM Port.VarMapValue
  evalCode mid code
    | languageIsSql mid = Port.Literal ∘ SQL2.array <$> runQuery code
    | otherwise = pure $ Port.QueryExpr code

  languageIsSql
    ∷ Maybe SDE.LanguageId
    → Boolean
  languageIsSql =
    maybe
      true
      ((_ ≡ "sql") ∘ S.toLower)

  extractSingletonObject
    ∷ SQL2.Literal
    → SQL2.Literal
  extractSingletonObject lit =
    case Mu.unroll lit of
      SQL2.Object obj →
        case SM.keys obj of
          [key] → fromMaybe lit $ SM.lookup key obj
          _ → lit
      _ → lit

  evalValue
    ∷ String
    → EvalM Port.VarMapValue
  evalValue code = do
    maybe (Port.Literal SQL2.null) (Port.Literal ∘ extractSingletonObject) ∘ A.head
      <$> runQuery code

  evalTextBox
    ∷ SD.TextBox (Const String)
    → EvalM (SD.TextBox Identity)
  evalTextBox tb = do
    let sql = getConst $ SD.traverseTextBox (map \_ → Const unit) tb
    mresult ← A.head <$> runQuery sql
    result ← maybe (Err.throwError $ Exn.error "No results") (pure ∘ extractSingletonObject) mresult
    case Tuple tb (Mu.unroll result) of
      Tuple (SD.PlainText _) (SQL2.String str) →
        pure ∘ SD.PlainText $ pure str
      Tuple (SD.Numeric _) (SQL2.Decimal a) →
        pure ∘ SD.Numeric $ pure a
      Tuple (SD.Time _) (SQL2.Time str) →
        SD.Time ∘ pure <$> parse parseSqlTime str
      Tuple (SD.Date _) (SQL2.Date str) →
        SD.Date ∘ pure <$> parse parseSqlDate str
      Tuple (SD.DateTime _) (SQL2.DateTime str) →
        SD.DateTime ∘ pure <$> parseSqlDateTime str
      Tuple _ res →
        Err.throwError ∘ Exn.error $ "Type error: " ⊕ show res ⊕ " does not match " ⊕ show tb
    where
      parse ∷ ∀ s a. P.Parser s a → s → EvalM a
      parse p str =
        case P.runParser str p of
          Left err → Err.throwError ∘ Exn.error $ "Parse error: " ⊕ show err
          Right a → pure a

  evalList
    ∷ String
    → EvalM (L.List Port.VarMapValue)
  evalList code = do
    items ← map extractSingletonObject <$> runQuery code
    let limit = 500
    pure ∘ L.toList $
      if A.length items > limit
        then map Port.Literal (A.take limit items) ⊕ [ SD.stringValue $ "<" ⊕ show limit ⊕ "item limit reached>" ]
        else Port.Literal <$> items

  rowToLiteral
    ∷ JSON.Json
    → Maybe SQL2.Literal
  rowToLiteral =
    either (\_ → Nothing) Just
      ∘ SQL2.decodeJsonLiteral

  runQuery
    ∷ String
    → EvalM (Array SQL2.Literal)
  runQuery code =
    case dir of
      Nothing → Err.throwError $ Exn.error "Cannot evaluate markdown without a saved notebook path"
      Just dir' → do
        n ← freshInt
        result ← Quasar.queryPrecise dir' code
        either
          (Err.throwError ∘ Exn.error)
          (pure ∘ A.mapMaybe rowToLiteral)
          result

parseDigit ∷ P.Parser String Int
parseDigit =
  PC.choice
    [ 0 <$ PS.string "0"
    , 1 <$ PS.string "1"
    , 2 <$ PS.string "2"
    , 3 <$ PS.string "3"
    , 4 <$ PS.string "4"
    , 5 <$ PS.string "5"
    , 6 <$ PS.string "6"
    , 7 <$ PS.string "7"
    , 8 <$ PS.string "8"
    , 9 <$ PS.string "9"
    ]

many1
  ∷ ∀ s a
  . P.Parser s a
  → P.Parser s (L.List a)
many1 p =
  L.Cons
    <$> p
    <*> L.many p

parseNat ∷ P.Parser String Int
parseNat =
  many1 parseDigit
    <#> F.foldl (\a i → a * 10 + i) 0

hyphen ∷ P.Parser String Unit
hyphen = void $ PS.string "-"

colon ∷ P.Parser String Unit
colon = void $ PS.string ":"

parseSqlDate ∷ P.Parser String SD.DateValue
parseSqlDate = do
  year ← parseNat
  hyphen
  month ← parseNat
  hyphen
  day ← parseNat
  pure { year, month, day }

parseSqlTime ∷ P.Parser String SD.TimeValue
parseSqlTime = do
  hours ← parseNat
  colon
  minutes ← parseNat
  colon
  _ ← parseNat -- seconds
  pure { hours, minutes }

-- | Parses a date-time string into the user's current locale.
parseSqlDateTime
  ∷ String
  → EvalM SD.DateTimeValue
parseSqlDateTime str = do
  case D.fromString str of
    Nothing → Err.throwError ∘ Exn.error $ "Invalid date: " ⊕ show str
    Just d → Eff.liftEff do
      D.Year year ← DL.year d
      month ← Enum.fromEnum <$> DL.month d
      D.DayOfMonth day ← DL.dayOfMonth d
      DT.HourOfDay hours ← DL.hourOfDay d
      DT.MinuteOfHour minutes ← DL.minuteOfHour d
      pure
        { date: { year , month , day }
        , time: { hours , minutes }
        }
