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

module Model.Notebook.Cell.Markdown where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (Json(), jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Optic.Core
import Text.Markdown.SlamDown (SlamDown(..), TextBoxType(..))
import Text.Markdown.SlamDown.Html (SlamDownState(..), initSlamDownState, FormFieldValue(..))
import qualified Data.StrMap as M
import qualified Data.Set as S
import qualified Model.Notebook.Cell.Common as C

newtype MarkdownRec =
  MarkdownRec { input :: String
              , state :: SlamDownState
              , evaluated :: Maybe SlamDown
              }

newtype EncFormField = EncFormField FormFieldValue

unEncFormField :: EncFormField -> FormFieldValue
unEncFormField (EncFormField e) = e

encodeTextBox :: TextBoxType -> Json
encodeTextBox PlainText = encodeJson "plainText"
encodeTextBox Date = encodeJson "date"
encodeTextBox Time = encodeJson "time"
encodeTextBox DateTime = encodeJson "dateTime"

decodeTextBox :: Json -> Either String TextBoxType
decodeTextBox j = do
  str <- decodeJson j
  case str of
    "plainText" -> pure PlainText
    "date" -> pure Date
    "time" -> pure Time
    "dateTime" -> pure DateTime
    _ -> Left "incorrect textbox type"

instance encodeEncFormField :: EncodeJson EncFormField where
  encodeJson (EncFormField (SingleValue tx str)) =
    "type" := encodeTextBox tx
    ~> "value" := str
    ~> jsonEmptyObject
  encodeJson (EncFormField (MultipleValues ss)) = encodeJson (S.toList ss)

instance decodeEncFormField :: DecodeJson EncFormField where
  decodeJson j =
    (EncFormField <$> (MultipleValues <$> (S.fromList <$> decodeJson j)))
    <|>
    (do obj <- decodeJson j
        tx <- (obj .? "type") >>= decodeTextBox
        v <- obj .? "value"
        pure $ EncFormField $ SingleValue tx v)

prepareToEnc :: SlamDownState -> M.StrMap EncFormField
prepareToEnc (SlamDownState m) = EncFormField <$> m

encodeState :: SlamDownState -> Json
encodeState = encodeJson <<< prepareToEnc

decodeState :: Json -> Either String SlamDownState
decodeState j = do
  m <- decodeJson j
  pure $ (SlamDownState (unEncFormField <$> m))

instance encodeJsonMarkdownRec :: EncodeJson MarkdownRec where
  encodeJson (MarkdownRec rec)
    =  "input" := rec.input
    ~> "state" := encodeState rec.state
    ~> jsonEmptyObject

instance decodeJsonMarkdownRec :: DecodeJson MarkdownRec where
  decodeJson json = do
    obj <- decodeJson json
    state <- obj .? "state"
    rec <- { input: _, state: _, evaluated: Nothing }
        <$> obj .? "input"
        <*> (decodeState state <|> pure emptySlamDownState)
    return $ MarkdownRec rec

initialMarkdownRec :: MarkdownRec
initialMarkdownRec =
  MarkdownRec { input: ""
              , state: emptySlamDownState
              , evaluated: Nothing
              }

_MarkdownRec :: LensP MarkdownRec _
_MarkdownRec = lens (\(MarkdownRec obj) -> obj) (const MarkdownRec)

_input :: LensP MarkdownRec String
_input = _MarkdownRec <<< C._input

_state :: LensP MarkdownRec SlamDownState
_state = _MarkdownRec <<< lens _.state _{state = _}

_evaluated :: LensP MarkdownRec (Maybe SlamDown)
_evaluated = _MarkdownRec <<< lens _.evaluated _{evaluated = _}

-- TODO: re-look at this, we may be able to simplify things elsewhere if we use `initSlamDownState` properly
emptySlamDownState :: SlamDownState
emptySlamDownState = initSlamDownState (SlamDown Nil)
