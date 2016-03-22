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

module SlamData.Notebook.Cell.Markdown.Model
  ( Model
  , encode
  , decode
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, jsonEmptyObject, encodeJson, decodeJson, (~>), (:=), (.?))
import Data.Set as S

import Text.Markdown.SlamDown (SlamDown(..), Block(..), Inline(..), ListType(..), CodeBlockType(..), LinkTarget(..), Expr(..), FormField(..), TextBoxType(..))
import Text.Markdown.SlamDown.Html (SlamDownFormState, FormFieldValue(..))

-- | The serialization model used for markdown cells.
type Model =
  { input :: SlamDown
  , state :: SlamDownFormState
  }

-- | Encodes the model as a JSON value.
encode :: Model -> Json
encode { input, state }
  = "input" := encodeSlamDown input
  ~> "state" := map encodeFormFieldValue state
  ~> jsonEmptyObject

-- | Attempts to decode a JSON value as the model.
decode :: Json -> Either String Model
decode = decodeJson >=> \obj -> do
  input <- decodeSlamDown =<< obj .? "input"
  state <- traverse decodeFormFieldValue =<< obj .? "state"
  pure { input, state }

encodeSlamDown :: SlamDown -> Json
encodeSlamDown (SlamDown bs)
  = "doc" := "slamdown"
  ~> "blocks" := map encodeBlock bs
  ~> jsonEmptyObject

decodeSlamDown :: Json -> Either String SlamDown
decodeSlamDown = decodeJson >=> \obj -> do
  docTag <- obj .? "doc"
  case docTag of
    "slamdown" -> SlamDown <$> (traverse decodeBlock =<< obj .? "blocks")
    _ -> Left $ "expected 'doc' to be 'slamdown', found '" ++ docTag ++ "'"

encodeBlock :: Block -> Json
encodeBlock (Paragraph is)
  = "type" := "para"
  ~> "content" := map encodeInline is
  ~> jsonEmptyObject
encodeBlock (Header level is)
  = "type" := "header"
  ~> "level" := level
  ~> "content" := map encodeInline is
  ~> jsonEmptyObject
encodeBlock (Blockquote bs)
  = "type" := "blockquote"
  ~> "content" := map encodeBlock bs
  ~> jsonEmptyObject
encodeBlock (Lst lt bss)
  = "type" := "list"
  ~> "listType" := encodeListType lt
  ~> "content" := map (map encodeBlock) bss
  ~> jsonEmptyObject
encodeBlock (CodeBlock cbt ls)
  = "type" := "codeblock"
  ~> "codeType" := encodeCodeBlockType cbt
  ~> "content" := ls
  ~> jsonEmptyObject
encodeBlock (LinkReference label uri)
  = "type" := "linkref"
  ~> "label" := label
  ~> "uri" := uri
  ~> jsonEmptyObject
encodeBlock Rule
  = "type" := "rule"
  ~> jsonEmptyObject

decodeBlock :: Json -> Either String Block
decodeBlock = decodeJson >=> \obj -> do
  ty <- obj .? "type"
  case ty of
    "para" -> Paragraph <$> (traverse decodeInline =<< obj .? "content")
    "header" -> Header <$> obj .? "level" <*> (traverse decodeInline =<< obj .? "content")
    "blockquote" -> Blockquote <$> (traverse decodeBlock =<< obj .? "content")
    "list" -> Lst <$> (decodeListType =<< obj .? "listType") <*> (traverse (traverse decodeBlock) =<< obj .? "content")
    "codeblock" -> CodeBlock <$> (decodeCodeBlockType =<< obj .? "codeType") <*> obj .? "content"
    "linkref" -> LinkReference <$> obj .? "label" <*> obj .? "uri"
    "rule" -> Right Rule
    _ -> Left ("unknown block type '" ++ ty ++ "'")

encodeInline :: Inline -> Json
encodeInline (Str s)
   = "type" := "str"
  ~> "value" := s
  ~> jsonEmptyObject
encodeInline (Entity e)
   = "type" := "entity"
  ~> "value" := e
  ~> jsonEmptyObject
encodeInline Space
   = "type" := "space"
  ~> jsonEmptyObject
encodeInline SoftBreak
   = "type" := "softbreak"
  ~> jsonEmptyObject
encodeInline LineBreak
   = "type" := "linebreak"
  ~> jsonEmptyObject
encodeInline (Emph is)
   = "type" := "em"
  ~> "value" := map encodeInline is
  ~> jsonEmptyObject
encodeInline (Strong is)
   = "type" := "strong"
  ~> "value" := map encodeInline is
  ~> jsonEmptyObject
encodeInline (Code evaluated code)
   = "type" := "code"
  ~> "evaluated" := evaluated
  ~> "value" := code
  ~> jsonEmptyObject
encodeInline (Link is lt)
   = "type" := "link"
  ~> "value" := map encodeInline is
  ~> "target" := encodeLinkTarget lt
  ~> jsonEmptyObject
encodeInline (Image is uri)
   = "type" := "image"
  ~> "value" := map encodeInline is
  ~> "uri" := uri
  ~> jsonEmptyObject
encodeInline (FormField label required field)
   = "type" := "field"
  ~> "label" := label
  ~> "required" := required
  ~> "field" := encodeFormField field
  ~> jsonEmptyObject

decodeInline :: Json -> Either String Inline
decodeInline = decodeJson >=> \obj -> do
  ty <- obj .? "type"
  case ty of
    "str" -> Str <$> obj .? "value"
    "entity" -> Entity <$> obj .? "value"
    "space" -> Right Space
    "softbreak" -> Right SoftBreak
    "linebreak" -> Right LineBreak
    "em" -> Emph <$> (traverse decodeInline =<< obj .? "value")
    "strong" -> Strong <$> (traverse decodeInline =<< obj .? "value")
    "code" -> Code <$> obj .? "evaluated" <*> obj .? "value"
    "link" -> Link <$> (traverse decodeInline =<< obj .? "value") <*> (decodeLinkTarget =<< obj .? "target")
    "image" -> Image <$> (traverse decodeInline =<< obj .? "value") <*> obj .? "uri"
    "field" -> FormField <$> obj .? "label" <*> obj .? "required" <*> (decodeFormField =<< obj .? "field")
    _ -> Left ("unknown inline type '" ++ ty ++ "'")

encodeListType :: ListType -> Json
encodeListType (Bullet s)
  = "type" := "bullet"
  ~> "inner" := s
  ~> jsonEmptyObject
encodeListType (Ordered s)
  = "type" := "ordered"
  ~> "inner" := s
  ~> jsonEmptyObject

decodeListType :: Json -> Either String ListType
decodeListType = decodeJson >=> \obj -> do
  ty <- obj .? "type"
  inner <- obj .? "inner"
  case ty of
    "bullet" -> Right (Bullet inner)
    "ordered" -> Right (Ordered inner)
    _ -> Left ("unknown list type '" ++ ty ++ "'")

encodeCodeBlockType :: CodeBlockType -> Json
encodeCodeBlockType Indented
   = "type" := "indented"
  ~> jsonEmptyObject
encodeCodeBlockType (Fenced evaluated info)
   = "type" := "fenced"
  ~> "evaluated" := evaluated
  ~> "info" := info
  ~> jsonEmptyObject

decodeCodeBlockType :: Json -> Either String CodeBlockType
decodeCodeBlockType = decodeJson >=> \obj -> do
  ty <- obj .? "type"
  case ty of
    "indented" -> Right Indented
    "fenced" -> Fenced <$> obj .? "evaluated" <*> obj .? "info"
    _ -> Left ("unknown code block type '" ++ ty ++ "'")

encodeLinkTarget :: LinkTarget -> Json
encodeLinkTarget (InlineLink uri)
   = "type" := "inline"
  ~> "uri" := uri
  ~> jsonEmptyObject
encodeLinkTarget (ReferenceLink ref)
   = "type" := "reference"
  ~> "ref" := ref
  ~> jsonEmptyObject

decodeLinkTarget :: Json -> Either String LinkTarget
decodeLinkTarget = decodeJson >=> \obj -> do
  ty <- obj .? "type"
  case ty of
    "inline" -> InlineLink <$> obj .? "uri"
    "reference" -> ReferenceLink <$> obj .? "ref"
    _ -> Left ("unknown code link target type '" ++ ty ++ "'")

encodeExpr :: forall a. (a -> Json) -> Expr a -> Json
encodeExpr enc (Literal a)
   = "type" := "lit"
  ~> "value" := enc a
  ~> jsonEmptyObject
encodeExpr _ (Unevaluated value)
   = "type" := "uneval"
  ~> "value" := value
  ~> jsonEmptyObject

decodeExpr :: forall a. (Json -> Either String a) -> Json -> Either String (Expr a)
decodeExpr dec = decodeJson >=> \obj -> do
  ty <- obj .? "type"
  case ty of
    "lit" -> Literal <$> (dec =<< obj .? "value")
    "uneval" -> Unevaluated <$> obj .? "value"
    _ -> Left ("unknown code expr type '" ++ ty ++ "'")

encodeFormField :: FormField -> Json
encodeFormField (TextBox tbt def)
   = "type" := "textbox"
  ~> "boxType" := encodeTextBoxType tbt
  ~> "default" := map (encodeExpr encodeJson) def
  ~> jsonEmptyObject
encodeFormField (RadioButtons sel ls)
   = "type" := "radios"
  ~> "selection" := encodeExpr encodeJson sel
  ~> "labels" := encodeExpr encodeJson ls
  ~> jsonEmptyObject
encodeFormField (CheckBoxes sel ls)
   = "type" := "checkboxes"
  ~> "selection" := encodeExpr encodeJson sel
  ~> "labels" := encodeExpr encodeJson ls
  ~> jsonEmptyObject
encodeFormField (DropDown opts def)
  = "type" := "dropdown"
  ~> "options" := encodeExpr encodeJson opts
  ~> "default" := map (encodeExpr encodeJson) def
  ~> jsonEmptyObject

decodeFormField :: Json -> Either String FormField
decodeFormField = decodeJson >=> \obj -> do
  ty <- obj .? "type"
  case ty of
    "textbox" -> TextBox <$> (decodeTextBoxType =<< obj .? "boxType") <*> (decodeMaybe (decodeExpr decodeJson) =<< obj .? "default")
    "radios" -> RadioButtons <$> (decodeExpr decodeJson =<< obj .? "selection") <*> (decodeExpr decodeJson =<< obj .? "labels")
    "checkboxes" -> CheckBoxes <$> (decodeExpr decodeJson =<< obj .? "selection") <*> (decodeExpr decodeJson =<< obj .? "labels")
    "dropdown" -> DropDown <$> (decodeExpr decodeJson =<< obj .? "options") <*> (decodeMaybe (decodeExpr decodeJson) =<< obj .? "default")
    _ -> Left ("unknown form field type '" ++ ty ++ "'")

-- | The decodeMaybe instance captures failing decoders in Nothing, not `null`
-- | values... even though it encodes `null` as `Nothing`.
decodeMaybe :: forall a. (Json -> Either String a) -> Json -> Either String (Maybe a)
decodeMaybe dec j = (Just <$> dec j) <|> pure Nothing

encodeTextBoxType :: TextBoxType -> Json
encodeTextBoxType tbt =
  encodeJson
    case tbt of
      PlainText -> "text"
      Numeric -> "num"
      Date -> "date"
      Time -> "time"
      DateTime -> "datetime"

decodeTextBoxType :: Json -> Either String TextBoxType
decodeTextBoxType = decodeJson >=> \tbt ->
  case tbt of
    "text" -> Right PlainText
    "num" -> Right Numeric
    "date" -> Right Date
    "time" -> Right Time
    "datetime" -> Right DateTime
    _ -> Left $ "unknown textbox type '" ++ tbt ++ "'"

encodeFormFieldValue :: FormFieldValue -> Json
encodeFormFieldValue (SingleValue tbt value)
   = "type" := "single"
  ~> "textboxType" := encodeTextBoxType tbt
  ~> "value" := value
  ~> jsonEmptyObject
encodeFormFieldValue (MultipleValues values)
   = "type" := "multi"
  ~> "values" := S.toList values
  ~> jsonEmptyObject

decodeFormFieldValue :: Json -> Either String FormFieldValue
decodeFormFieldValue = decodeJson >=> \obj -> do
  ty <- obj .? "type"
  case ty of
    "single" -> SingleValue <$> (decodeTextBoxType =<< obj .? "textboxType") <*> obj .? "value"
    "multi" -> MultipleValues <$> (S.fromList <$> obj .? "values")
    _ -> Left $ "unknown form field value type '" ++ ty ++ "'"
