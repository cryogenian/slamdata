module Model.Notebook.Cell.JTableContent where

import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (Json(), jsonEmptyObject)
import Data.Argonaut.Encode (EncodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Int (Int(), fromNumber)
import Data.Maybe (Maybe(..))
import Optic.Core (LensP(), lens)

newtype JTableContent =
  JTableContent { perPage :: Int
                , page :: Int
                , result :: Maybe Result
                }

initialJTableContent :: JTableContent
initialJTableContent =
  JTableContent { perPage: fromNumber 10
                , page: fromNumber 1
                , result: Nothing
                }

_jTableContent :: LensP JTableContent _
_jTableContent = lens (\(JTableContent obj) -> obj) (const JTableContent)

_perPage :: LensP JTableContent Int
_perPage = _jTableContent <<< lens _.perPage (_ { perPage = _ })

_page :: LensP JTableContent Int
_page = _jTableContent <<< lens _.page (_ { page = _ })

_result :: LensP JTableContent (Maybe Result)
_result = _jTableContent <<< lens _.result (_ { result = _ })

instance encodeJsonJTableContent :: EncodeJson JTableContent where
  encodeJson (JTableContent rec)
    =  "perPage" := rec.perPage
    ~> "page" := rec.page
    ~> jsonEmptyObject

instance decodeJsonJTableContent :: DecodeJson JTableContent where
  decodeJson json = do
    obj <- decodeJson json
    rec <- { perPage: _, page: _, result: Nothing }
        <$> obj .? "perPage"
        <*> obj .? "page"
    return $ JTableContent rec

newtype Result =
  Result { totalPages :: Int
         , values :: Maybe Json
         }

_resultRec :: LensP Result _
_resultRec = lens (\(Result obj) -> obj) (const Result)

_totalPages :: LensP Result Int
_totalPages = _resultRec <<< lens _.totalPages (_ { totalPages = _ })

_values :: LensP Result (Maybe Json)
_values = _resultRec <<< lens _.values (_ { values = _ })
