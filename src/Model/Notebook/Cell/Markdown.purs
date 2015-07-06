module Model.Notebook.Cell.Markdown where

import Control.Alt ((<|>))
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (Json(), jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Either (Either(..))
import Optic.Core (LensP(), lens)
import Text.Markdown.SlamDown (SlamDown(..), TextBoxType(..))
import Text.Markdown.SlamDown.Html (SlamDownState(..), initSlamDownState, FormFieldValue(..))
import qualified Data.StrMap as M
import qualified Data.Set as S
import qualified Model.Notebook.Cell.Common as C


newtype MarkdownRec =
  MarkdownRec { input :: String
              , state :: SlamDownState
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
    rec <- { input: _, state: _ }
        <$> obj .? "input"
        <*> (decodeState state <|> pure emptySlamDownState)
    return $ MarkdownRec rec

initialMarkdownRec :: MarkdownRec
initialMarkdownRec =
  MarkdownRec { input: ""
              , state: emptySlamDownState
              }

_MarkdownRec :: LensP MarkdownRec _
_MarkdownRec = lens (\(MarkdownRec obj) -> obj) (const MarkdownRec)

_input :: LensP MarkdownRec String
_input = _MarkdownRec <<< C._input

_state :: LensP MarkdownRec SlamDownState
_state = _MarkdownRec <<< lens _.state _{state = _}

-- TODO: re-look at this, we may be able to simplify things elsewhere if we use `initSlamDownState` properly
emptySlamDownState :: SlamDownState
emptySlamDownState = initSlamDownState (SlamDown [])
