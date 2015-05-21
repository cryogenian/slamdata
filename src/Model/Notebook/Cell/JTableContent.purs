module Model.Notebook.Cell.JTableContent where

import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (Json(), jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Either (Either(..), either)
import Data.Int (Int(), fromNumber)
import Data.Maybe (Maybe(..))
import Data.These (These(..), these)
import Optic.Core (LensP(), lens)

newtype JTableContent =
  JTableContent { perPage :: Either (These String Int) Int
                , page :: These String Int
                , result :: Maybe Result
                }

initialJTableContent :: JTableContent
initialJTableContent =
  JTableContent { perPage: Right Config.defaultPageSize
                , page: That one
                , result: Nothing
                }

_jTableContent :: LensP JTableContent _
_jTableContent = lens (\(JTableContent obj) -> obj) (const JTableContent)

_perPage :: LensP JTableContent (Either (These String Int) Int)
_perPage = _jTableContent <<< lens _.perPage (_ { perPage = _ })

_page :: LensP JTableContent (These String Int)
_page = _jTableContent <<< lens _.page (_ { page = _ })

_result :: LensP JTableContent (Maybe Result)
_result = _jTableContent <<< lens _.result (_ { result = _ })

instance encodeJsonJTableContent :: EncodeJson JTableContent where
  encodeJson (JTableContent rec)
    =  "perPage" := either readThese id rec.perPage
    ~> "page" := readThese rec.page
    ~> jsonEmptyObject
    where
    readThese :: These String Int -> Int
    readThese = these (const one) id (\_ n -> n)

instance decodeJsonJTableContent :: DecodeJson JTableContent where
  decodeJson json = do
    obj <- decodeJson json
    rec <- { perPage: _, page: _, result: Nothing }
        <$> (Right <$> obj .? "perPage")
        <*> (That <$> obj .? "page")
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
