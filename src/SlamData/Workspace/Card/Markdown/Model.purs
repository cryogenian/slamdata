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

module SlamData.Workspace.Card.Markdown.Model
  ( Model
  , encode
  , decode
  , eqModel
  , genModel
  , emptyModel
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, JObject, jsonEmptyObject, encodeJson, decodeJson, (~>), (:=), (.?))
import Data.DateTime as DT
import Data.Enum (fromEnum, toEnum)
import Data.Functor.Compose (Compose(..))
import Data.HugeNum as HN
import Data.StrMap as SM
import Data.Traversable as T

import SlamData.Workspace.Card.Port.VarMap as VM

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.State as SDS

-- | The serialization model used for markdown cards.
type Model =
  { input ∷ SD.SlamDownP VM.VarMapValue
  , state ∷ SDS.SlamDownFormState VM.VarMapValue
  }

genModel ∷ Gen.Gen Model
genModel = do
  input ← SC.arbitrary
  state ← SM.fromFoldable <$> Gen.arrayOf (Tuple <$> SC.arbitrary <*> SC.arbitrary)
  pure { input, state }

eqModel ∷ Model → Model → Boolean
eqModel m1 m2 =
  m1.input ≡ m2.input
    && m1.state ≡ m2.state

emptyModel ∷ Model
emptyModel =
  { input: SD.SlamDown mempty
  , state: SM.empty
  }

-- | For getting parse error messages that are suitable for diagnostics.
traceError
  ∷ ∀ a
  . String
  → Either String a
  → Either String a
traceError lbl e =
  case e of
    Right x → Right x
    Left err → Left $ lbl ⊕ " » " ⊕ err

-- | Encodes the model as a JSON value.
encode
  ∷ Model
  → Json
encode { input, state }
  = "input" := encodeSlamDown input
  ~> "state" := map encodeFormFieldValue state
  ~> jsonEmptyObject

-- | Attempts to decode a JSON value as the model.
decode
  ∷ Json
  → Either String Model
decode =
  decodeJson >=> \obj → do
    input ← decodeSlamDown =<< obj .? "input"
    state ← traverse decodeFormFieldValue =<< obj .? "state"
    pure { input, state }

encodeSlamDown
  ∷ SD.SlamDownP VM.VarMapValue
  → Json
encodeSlamDown (SD.SlamDown bs)
  = "doc" := "slamdown"
  ~> "blocks" := map encodeBlock bs
  ~> jsonEmptyObject

decodeSlamDown
  ∷ Json
  → Either String (SD.SlamDownP VM.VarMapValue)
decodeSlamDown =
  decodeJson >=> \obj → do
    docTag ← obj .? "doc"
    case docTag of
      "slamdown" → SD.SlamDown <$> (traverse decodeBlock =<< obj .? "blocks")
      _ → Left $ "expected 'doc' to be 'slamdown', found '" ⊕ docTag ⊕ "'"

encodeBlock
  ∷ SD.Block VM.VarMapValue
  → Json
encodeBlock (SD.Paragraph is)
  = "type" := "para"
  ~> "content" := map encodeInline is
  ~> jsonEmptyObject
encodeBlock (SD.Header level is)
  = "type" := "header"
  ~> "level" := level
  ~> "content" := map encodeInline is
  ~> jsonEmptyObject
encodeBlock (SD.Blockquote bs)
  = "type" := "blockquote"
  ~> "content" := map encodeBlock bs
  ~> jsonEmptyObject
encodeBlock (SD.Lst lt bss)
  = "type" := "list"
  ~> "listType" := encodeListType lt
  ~> "content" := map (map encodeBlock) bss
  ~> jsonEmptyObject
encodeBlock (SD.CodeBlock cbt ls)
  = "type" := "codeblock"
  ~> "codeType" := encodeCodeBlockType cbt
  ~> "content" := ls
  ~> jsonEmptyObject
encodeBlock (SD.LinkReference label uri)
  = "type" := "linkref"
  ~> "label" := label
  ~> "uri" := uri
  ~> jsonEmptyObject
encodeBlock SD.Rule
  = "type" := "rule"
  ~> jsonEmptyObject

decodeBlock
  ∷ Json
  → Either String (SD.Block VM.VarMapValue)
decodeBlock =
  decodeJson >=> \obj → do
    ty ← obj .? "type"
    case ty of
      "para" →
        SD.Paragraph
          <$> (traverse decodeInline =<< obj .? "content")
      "header" →
        SD.Header
          <$> obj .? "level"
          <*> (traverse decodeInline =<< obj .? "content")
      "blockquote" →
        SD.Blockquote
          <$> (traverse decodeBlock =<< obj .? "content")
      "list" →
        SD.Lst
          <$> (decodeListType =<< obj .? "listType")
          <*> (traverse (traverse decodeBlock) =<< obj .? "content")
      "codeblock" →
        SD.CodeBlock
          <$> (decodeCodeBlockType =<< obj .? "codeType")
          <*> obj .? "content"
      "linkref" →
        SD.LinkReference
          <$> obj .? "label"
          <*> obj .? "uri"
      "rule" → pure SD.Rule
      _ → Left $ "unknown block type '" ⊕ ty ⊕ "'"

encodeInline
  ∷ SD.Inline VM.VarMapValue
  → Json
encodeInline (SD.Str s)
   = "type" := "str"
  ~> "value" := s
  ~> jsonEmptyObject
encodeInline (SD.Entity e)
   = "type" := "entity"
  ~> "value" := e
  ~> jsonEmptyObject
encodeInline SD.Space
   = "type" := "space"
  ~> jsonEmptyObject
encodeInline SD.SoftBreak
   = "type" := "softbreak"
  ~> jsonEmptyObject
encodeInline SD.LineBreak
   = "type" := "linebreak"
  ~> jsonEmptyObject
encodeInline (SD.Emph is)
   = "type" := "em"
  ~> "value" := map encodeInline is
  ~> jsonEmptyObject
encodeInline (SD.Strong is)
   = "type" := "strong"
  ~> "value" := map encodeInline is
  ~> jsonEmptyObject
encodeInline (SD.Code evaluated code)
   = "type" := "code"
  ~> "evaluated" := evaluated
  ~> "value" := code
  ~> jsonEmptyObject
encodeInline (SD.Link is lt)
   = "type" := "link"
  ~> "value" := map encodeInline is
  ~> "target" := encodeLinkTarget lt
  ~> jsonEmptyObject
encodeInline (SD.Image is uri)
   = "type" := "image"
  ~> "value" := map encodeInline is
  ~> "uri" := uri
  ~> jsonEmptyObject
encodeInline (SD.FormField label required field)
   = "type" := "field"
  ~> "label" := label
  ~> "required" := required
  ~> "field" := encodeFormField field
  ~> jsonEmptyObject

decodeInline
  ∷ Json
  → Either String (SD.Inline VM.VarMapValue)
decodeInline =
  decodeJson >=> \obj → do
    ty ← obj .? "type"
    case ty of
      "str" → SD.Str <$> obj .? "value"
      "entity" → SD.Entity <$> obj .? "value"
      "space" → pure SD.Space
      "softbreak" → pure SD.SoftBreak
      "linebreak" → pure SD.LineBreak
      "em" → SD.Emph <$> (traverse decodeInline =<< obj .? "value")
      "strong" → SD.Strong <$> (traverse decodeInline =<< obj .? "value")
      "code" →
        SD.Code
          <$> obj .? "evaluated"
          <*> obj .? "value"
      "link" →
        SD.Link
          <$> (traverse decodeInline =<< obj .? "value")
          <*> (decodeLinkTarget =<< obj .? "target")
      "image" →
        SD.Image
          <$> (traverse decodeInline =<< obj .? "value")
          <*> obj .? "uri"
      "field" →
        SD.FormField
          <$> obj .? "label"
          <*> obj .? "required"
          <*> (decodeFormField =<< obj .? "field")
      _ → Left $ "unknown inline type '" ⊕ ty ⊕ "'"

encodeListType
  ∷ SD.ListType
  → Json
encodeListType (SD.Bullet s)
  = "type" := "bullet"
  ~> "inner" := s
  ~> jsonEmptyObject
encodeListType (SD.Ordered s)
  = "type" := "ordered"
  ~> "inner" := s
  ~> jsonEmptyObject

decodeListType
  ∷ Json
  → Either String SD.ListType
decodeListType =
  decodeJson >=> \obj → do
    ty ← obj .? "type"
    inner ← obj .? "inner"
    case ty of
      "bullet" → pure $ SD.Bullet inner
      "ordered" → pure $ SD.Ordered inner
      _ → Left ("unknown list type '" ⊕ ty ⊕ "'")

encodeCodeBlockType
  ∷ SD.CodeBlockType
  → Json
encodeCodeBlockType SD.Indented
   = "type" := "indented"
  ~> jsonEmptyObject
encodeCodeBlockType (SD.Fenced evaluated info)
   = "type" := "fenced"
  ~> "evaluated" := evaluated
  ~> "info" := info
  ~> jsonEmptyObject

decodeCodeBlockType
  ∷ Json
  → Either String SD.CodeBlockType
decodeCodeBlockType =
  decodeJson >=> \obj → do
    ty ← obj .? "type"
    case ty of
      "indented" → pure SD.Indented
      "fenced" → SD.Fenced <$> obj .? "evaluated" <*> obj .? "info"
      _ → Left $ "unknown code block type '" ⊕ ty ⊕ "'"

encodeLinkTarget
  ∷ SD.LinkTarget
  → Json
encodeLinkTarget (SD.InlineLink uri)
   = "type" := "inline"
  ~> "uri" := uri
  ~> jsonEmptyObject
encodeLinkTarget (SD.ReferenceLink ref)
   = "type" := "reference"
  ~> "ref" := ref
  ~> jsonEmptyObject

decodeLinkTarget
  ∷ Json
  → Either String SD.LinkTarget
decodeLinkTarget =
  decodeJson >=> \obj → do
    ty ← obj .? "type"
    case ty of
      "inline" → SD.InlineLink <$> obj .? "uri"
      "reference" → SD.ReferenceLink <$> obj .? "ref"
      _ → Left $ "unknown code link target type '" ⊕ ty ⊕ "'"

encodeExpr
  ∷ ∀ a
  . (a → Json)
  → SD.Expr a
  → Json
encodeExpr enc expr =
  case expr of
    SD.Literal a →
      "type" := "lit"
        ~> "value" := enc a
        ~> jsonEmptyObject
    SD.Unevaluated value →
      "type" := "uneval"
        ~> "value" := value
        ~> jsonEmptyObject

decodeExpr
  ∷ ∀ a
  . (Json → Either String a)
  → Json
  → Either String (SD.Expr a)
decodeExpr rec json =
  traceError ("[expr: " ⊕ show json ⊕ "]") do
    obj ← decodeJson json
    ty ← traceError "type" $ obj .? "type"
    traceError ty
      case ty of
        "lit" → SD.Literal <$> (rec =<< obj .? "value")
        "uneval" → SD.Unevaluated <$> obj .? "value"
        _ → Left $ "unknown code expr type '" ⊕ ty ⊕ "'"

encodeFormField
  ∷ SD.FormField VM.VarMapValue
  → Json
encodeFormField field =
  case field of
    SD.TextBox tb →
      "type" := "textbox"
        ~> "textBox" := encodeTextBox tb
        ~> jsonEmptyObject
    SD.RadioButtons x xs →
      "type" := "radios"
        ~> "selection" := encodeExpr encodeJson x
        ~> "labels" := encodeExpr encodeJson xs
        ~> jsonEmptyObject
    SD.CheckBoxes bs xs →
      "type" := "checkboxes"
        ~> "selection" := encodeExpr encodeJson bs
        ~> "labels" := encodeExpr encodeJson xs
        ~> jsonEmptyObject
    SD.DropDown mx xs →
      "type" := "dropdown"
        ~> "selection" := map (encodeExpr encodeJson) mx
        ~> "labels" := encodeExpr encodeJson xs
        ~> jsonEmptyObject


decodeFormField
  ∷ Json
  → Either String (SD.FormField VM.VarMapValue)
decodeFormField =
  decodeJson >=> \obj → do
    ty ← obj .? "type"
    traceError ty
      case ty of
        "textbox" → do
          tb ← obj .? "textBox" >>= decodeTextBox
          pure $ SD.TextBox tb
        "radios" → do
          selection ← traceError "selection" $ obj .? "selection" >>= decodeExpr decodeJson
          labels ← traceError "labels" $ obj .? "labels" >>= decodeExpr decodeJson
          pure $ SD.RadioButtons selection labels
        "checkboxes" → do
          selection ← traceError "selection" $ obj .? "selection" >>= decodeExpr decodeJson
          labels ← traceError "labels" $ obj .? "labels" >>= decodeExpr decodeJson
          pure $ SD.CheckBoxes selection labels
        "dropdown" → do
          selection ← traceError "selection" $ obj .? "selection" >>= decodeMaybe (decodeExpr decodeJson)
          labels ← traceError "labels" $ obj .? "labels" >>= decodeExpr decodeJson
          pure $ SD.DropDown selection labels
        _ → Left $ "Unknown form field type '" ⊕ ty ⊕ "'"

decodeMaybe
  ∷ ∀ a
  . (Json → Either String a)
  → Json
  → Either String (Maybe a)
decodeMaybe dec j =
  (Just <$> dec j)
    <|> pure Nothing

decodeTextBox
  ∷ Json
  → Either String (SD.TextBox (Compose Maybe SD.Expr))
decodeTextBox =
  decodeJson >=> \obj → do
    ty ← obj .? "type"
    value ← obj .? "value"
    traceError ty
      case ty of
        "plaintext" →
          SD.PlainText ∘ Compose <$>
            T.traverse (decodeExpr decodeJson) value
        "numeric" →
          SD.Numeric ∘ Compose <$>
            T.traverse (decodeExpr decodeHugeNum) value
        "datetime" →
          SD.DateTime (decodePrecision obj) ∘ Compose <$>
            T.traverse (decodeExpr decodeDateTime) value
        "date" →
          SD.Date ∘ Compose <$>
            T.traverse (decodeExpr decodeDate) value
        "time" →
          SD.Time (decodePrecision obj) ∘ Compose <$>
            T.traverse (decodeExpr decodeTime) value
        _ → Left $ "Unknown text box type '" ⊕ ty ⊕ "'"

  where
    decodeHugeNum ∷ Json → Either String HN.HugeNum
    decodeHugeNum =
      decodeJson
        >=> HN.fromString
        >>> maybe (Left "Error decoding number") Right

    decodeDateTime ∷ Json → Either String DT.DateTime
    decodeDateTime =
      decodeJson >=> \obj →
        DT.DateTime
          <$> (obj .? "date" >>= decodeDate)
          <*> (obj .? "time" >>= decodeTime)

    decodeDate ∷ Json → Either String DT.Date
    decodeDate =
      decodeJson >=> \obj → do
        year ← obj .? "year"
        month ← obj .? "month"
        day ← obj .? "day"
        maybe (Left "Invalid date value") Right $
          join $ DT.exactDate <$> toEnum year <*> toEnum month <*> toEnum day

    decodeTime ∷ Json → Either String DT.Time
    decodeTime =
      decodeJson >=> \obj → do
        hours ← obj .? "hours"
        minutes ← obj .? "minutes"
        seconds ← (obj .? "seconds") <|> pure Nothing
        maybe (Left "Invalid time value") Right $
          DT.Time <$> toEnum hours <*> toEnum minutes <*> toEnum (fromMaybe 0 seconds) <*> pure bottom

    decodePrecision ∷ JObject -> SD.TimePrecision
    decodePrecision obj =
      if either (const false) id $ (_ == "seconds") <$> obj .? "prec"
      then SD.Seconds
      else SD.Minutes


-- Note: we use strings to represent numeric values to avoid losing precision
encodeTextBox
  ∷ SD.TextBox (Compose Maybe SD.Expr)
  → Json
encodeTextBox tb =
  case tb of
    SD.PlainText (Compose x) →
      "type" := "plaintext"
        ~> "value" := map (encodeExpr encodeJson) x
        ~> jsonEmptyObject
    SD.Numeric (Compose x) →
      "type" := "numeric"
        ~> "value" := map (encodeExpr (encodeJson ∘ HN.toString)) x
        ~> jsonEmptyObject
    SD.DateTime prec (Compose x) →
      "type" := "datetime"
        ~> "value" := map (encodeExpr encodeDateTime) x
        ~> "prec" := encodePrecision prec
        ~> jsonEmptyObject
    SD.Date (Compose x) →
      "type" := "date"
        ~> "value" := map (encodeExpr encodeDate) x
        ~> jsonEmptyObject
    SD.Time prec (Compose x) →
      "type" := "time"
        ~> "value" := map (encodeExpr encodeTime) x
        ~> "prec" := encodePrecision prec
        ~> jsonEmptyObject

  where
    encodeDate d =
      "year" := fromEnum (DT.year d)
        ~> "month" := fromEnum (DT.month d)
        ~> "day" := fromEnum (DT.day d)
        ~> jsonEmptyObject

    encodeTime t =
      "hours" := fromEnum (DT.hour t)
        ~> "minutes" := fromEnum (DT.minute t)
        ~> "seconds" := fromEnum (DT.second t)
        ~> jsonEmptyObject

    encodeDateTime (DT.DateTime date time) =
      "date" := encodeDate date
        ~> "time" := encodeTime time
        ~> jsonEmptyObject

    encodePrecision =
      encodeJson ∘ case _ of
        SD.Seconds -> "seconds"
        SD.Minutes -> "minutes"


encodeFormFieldValue
  ∷ SDS.FormFieldValue VM.VarMapValue
  → Json
encodeFormFieldValue =
  encodeFormField
    ∘ SD.transFormField (SD.Literal ∘ unwrap)

decodeFormFieldValue
  ∷ Json
  → Either String (SDS.FormFieldValue VM.VarMapValue)
decodeFormFieldValue json = do
  field ← decodeFormField json
  SD.traverseFormField (map pure ∘ SD.getLiteral) field
    # maybe (Left "Invalid form field value") pure
