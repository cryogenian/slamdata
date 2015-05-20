module Controller.Notebook.Cell.JTableContent
  ( goPage
  , stepPage
  , updatePage
  , loadPage
  , changePageSize
  , runJTable
  ) where

import Api.Query (query, sample)
import Control.Bind ((<=<), (>=>))
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.Notebook.Common (I())
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (Json(), JObject(), fromArray, toObject, toNumber, fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (head)
import Data.Date (now)
import Data.These (These(..), these, theseRight, thisOrBoth)
import Data.Either (Either(..))
import Data.Either.Unsafe (fromRight)
import Data.Foreign.Class (readJSON)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Global (isNaN, readInt)
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(..), CellResultContent(..))
import Model.Notebook.Cell (Cell(), _JTableContent, _content, _cellId)
import Model.Resource (Resource())
import Optic.Core ((^.), (.~), (..))
import Optic.Extended (TraversalP(), (^?))

import qualified Model.Notebook.Cell.JTableContent as JTC
import qualified Data.Int as I
import qualified Data.StrMap as SM

_page :: TraversalP Cell (These String I.Int)
_page = _content .. _JTableContent .. JTC._page

currentPage :: Cell -> Maybe I.Int
currentPage cell = maybe Nothing theseRight (cell ^? _page)

goPage :: forall e. I.Int -> Cell -> (Cell -> I e) -> I e
goPage page cell run =
  ((RunCell (cell ^. _cellId)) <$> liftEff now) `andThen`
    \_ -> run (cell # _page .~ That page)

stepPage :: forall e. I.Int -> Cell -> (Cell -> I e) -> I e
stepPage delta cell = goPage (maybe one (delta +) (currentPage cell)) cell

updatePage :: forall e. Cell -> String -> I e
updatePage cell val =
  pure $ UpdateCell (cell ^. _cellId) (_page .~ thisOrBoth val (currentPage cell))

loadPage :: forall e. Cell -> (Cell -> I e) -> I e
loadPage cell run = case cell ^? _content .. _JTableContent of
  Nothing -> empty
  Just table ->
    goPage (these (readPageNum one) id (flip readPageNum) (table ^. JTC._page)) cell run
    where
    readPageNum :: I.Int -> String -> I.Int
    readPageNum default str =
      let num = readInt 10 str
      in if isNaN num then default else I.fromNumber num

changePageSize :: forall e. Cell -> (Cell -> I e) -> String -> I e
changePageSize cell run value = case readJSON value of
  Left _ -> empty
  Right n ->
    ((RunCell (cell ^. _cellId)) <$> liftEff now) `andThen`
      \_ -> run (cell # (_content .. _JTableContent .. JTC._perPage .~ I.fromNumber n)
                     .. (_page .~ That one))

runJTable :: forall e. Resource -> Cell -> I e
runJTable file cell = fromMaybe empty $ do
  table <- cell ^? _content .. _JTableContent
  return $ do
    let perPage = table ^. JTC._perPage
        pageNumber = fromMaybe one $ theseRight (table ^. JTC._page)
        pageIndex = pageNumber - one
    -- TODO: catch aff failures?
    numItems <- liftAff $ fromMaybe 0 <<< readTotal <$> query file "SELECT COUNT(*) AS total FROM {{path}}"
    result <- liftAff $ sample file (pageIndex * perPage) perPage
    now' <- liftEff now
    return $ CellResult (cell ^. _cellId) now' $ Right $ JTableContent $
      JTC.JTableContent { perPage: perPage
                        , page: That pageNumber
                        , result: Just $ JTC.Result
                          { totalPages: I.fromNumber $ Math.ceil (numItems / I.toNumber perPage)
                          , values: Just $ fromArray (readValue <$> result)
                          }
                        }
  where
  readTotal :: [Json] -> Maybe Number
  readTotal = toNumber <=< SM.lookup "total" <=< toObject <=< head

  -- temporary files are written to `value` field or even `value.value`
  readValue :: Json -> Json
  readValue json = fromObject $ fromRight do
    obj <- decodeJson json
    if SM.keys obj == ["value"]
      then let value :: Either _ JObject
               value = obj .? "value" in
           if SM.keys <$> value == pure ["value"]
           then (.? "value") >=> (.? "value") $ obj
           else value
      else pure obj

