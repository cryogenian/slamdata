module Model.Notebook.Cell.Explore
  ( ExploreRec()
  , initialExploreRec
  , _input
  , _table
  ) where

import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Model.Notebook.Cell.FileInput (FileInput(), initialFileInput)
import Model.Notebook.Cell.JTableContent (JTableContent(), initialJTableContent)
import Optic.Core (LensP(), lens)

import qualified Model.Notebook.Cell.Common as C

newtype ExploreRec =
  ExploreRec { input :: FileInput
             , table :: JTableContent
             }

initialExploreRec :: ExploreRec
initialExploreRec =
  ExploreRec { input: initialFileInput
             , table: initialJTableContent
             }

_ExploreRec :: LensP ExploreRec _
_ExploreRec = lens (\(ExploreRec obj) -> obj) (const ExploreRec)

_input :: LensP ExploreRec FileInput
_input = _ExploreRec <<< C._input

_table :: LensP ExploreRec JTableContent
_table = _ExploreRec <<< C._table

instance encodeJsonExploreRec :: EncodeJson ExploreRec where
  encodeJson (ExploreRec rec)
    =  "input" := rec.input
    ~> "table" := rec.table
    ~> jsonEmptyObject

instance decodeJsonExploreRec :: DecodeJson ExploreRec where
  decodeJson json = do
    obj <- decodeJson json
    rec <- { input: _, table: _ }
        <$> obj .? "input"
        <*> obj .? "table"
    return $ ExploreRec rec
