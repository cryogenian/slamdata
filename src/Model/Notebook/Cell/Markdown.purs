module Model.Notebook.Cell.Markdown where

import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (Json(), jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Either (Either())
import Optic.Core (LensP(), lens)
import Text.Markdown.SlamDown.Html (SlamDownState(), emptySlamDownState)
import qualified Model.Notebook.Cell.Common as C

newtype MarkdownRec =
  MarkdownRec { input :: String
              , state :: SlamDownState
              }

encodeState :: SlamDownState -> Json
encodeState _ = jsonEmptyObject

decodeState :: Json -> Either String SlamDownState
decodeState _ = pure emptySlamDownState

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
        <*> decodeState state
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
