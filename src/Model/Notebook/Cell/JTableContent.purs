module Model.Notebook.Cell.JTableContent
  ( JTableContent(..)
  , initialJTableContent
  , _page
  , _perPage
  , _result
  , Result(..)
  , _totalPages
  , _values
  ) where

import Control.Alt ((<|>))
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (Json(), jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Either (Either(..), either)
import Data.Int (Int(), fromNumber)
import Data.Maybe (Maybe(..))
import Data.These (These(..), these)
import Optic.Core (LensP(), lens)

-- | The current JTable pagination parameters and loaded JSON values if present.
newtype JTableContent =
  JTableContent { page :: These String Int
                , perPage :: Either (These String Int) Int
                , result :: Maybe Result
                }

-- | The initial JTable value used when constructing a cell with JTableContent
-- | output.
initialJTableContent :: JTableContent
initialJTableContent =
  JTableContent { perPage: Right Config.defaultPageSize
                , page: That one
                , result: Nothing
                }

-- | Lens for the JTableContent newtype.
_JTableContent :: LensP JTableContent _
_JTableContent = lens (\(JTableContent obj) -> obj) (const JTableContent)

-- | The current page number. The `This` part of the value is used
-- | when a value has been typed in the page number input field but not yet
-- | "accepted". The `That` is used for the actual current page value.
_page :: LensP JTableContent (These String Int)
_page = _JTableContent <<< lens _.page (_ { page = _ })

-- | The maximum number of items to show in the table at any one
-- | time. A `Right` value indicates a choice has been made from the dropdown
-- | list of preset sizes. A `Left` value indicates a custom page size is
-- | desired, and the `This` and `That` parts work much as with the `page`
-- | value.
_perPage :: LensP JTableContent (Either (These String Int) Int)
_perPage = _JTableContent <<< lens _.perPage (_ { perPage = _ })

-- | The current result values used to render a JTable.
_result :: LensP JTableContent (Maybe Result)
_result = _JTableContent <<< lens _.result (_ { result = _ })

instance encodeJsonJTableContent :: EncodeJson JTableContent where
  encodeJson (JTableContent rec)
    =  "perPage" := either readThese id rec.perPage
    ~> "page" := readThese rec.page
    ~> "result" := rec.result
    ~> jsonEmptyObject
    where
    readThese :: These String Int -> Int
    readThese = these (const one) id (\_ n -> n)

instance decodeJsonJTableContent :: DecodeJson JTableContent where
  decodeJson json = do
    obj <- decodeJson json
    rec <- { perPage: _, page: _, result: _ }
        <$> (Right <$> obj .? "perPage")
        <*> (That <$> obj .? "page")
        <*> ((obj .? "result") <|> pure Nothing)
    return $ JTableContent rec

-- | The current result values used to render a JTable.
newtype Result =
  Result { totalPages :: Int
         , values :: Maybe Json
         }

instance encodeResult :: EncodeJson Result where
  encodeJson (Result r)
   =  "totalPages" := r.totalPages
   ~> "values" := r.values
   ~> jsonEmptyObject

instance decodeResult :: DecodeJson Result where
  decodeJson json = do
    obj <- decodeJson json
    r <- {totalPages: _, values: _}
         <$> obj .? "totalPages"
         <*> obj .? "values"
    pure $ Result r
    

-- | Lens for the Result newtype.
_Result :: LensP Result _
_Result = lens (\(Result obj) -> obj) (const Result)

-- | The total number of pages available for the current pagination settings.
_totalPages :: LensP Result Int
_totalPages = _Result <<< lens _.totalPages (_ { totalPages = _ })

-- | The result from the server.
_values :: LensP Result (Maybe Json)
_values = _Result <<< lens _.values (_ { values = _ })
