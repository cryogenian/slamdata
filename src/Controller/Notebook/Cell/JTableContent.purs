module Controller.Notebook.Cell.JTableContent
  ( goPage
  , stepPage
  , inputPage
  , inputPageSize
  , loadPage
  , changePageSize
  , runJTable
  , queryToJTable
  ) where

import Api.Fs (delete)
import Api.Query (port, query, sample)
import Control.Bind ((<=<), (>=>))
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Plus (empty)
import Controller.Notebook.Common (I(), run, update, finish)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (Json(), JObject(), fromArray, toObject, toNumber, fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (head)
import Data.Date (now)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Foreign.Class (readJSON)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.These (These(..), these, theseRight, thisOrBoth)
import Global (isNaN, readInt)
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(..), CellResultContent(..))
import Model.Notebook.Cell
import Model.Resource (Resource())
import Optic.Core ((^.), (.~), (..))
import Optic.Extended (TraversalP(), (^?))

import qualified Data.Array.NonEmpty as NEL
import qualified Data.Int as I
import qualified Data.StrMap as SM
import qualified Model.Notebook.Cell.JTableContent as JTC

_page :: TraversalP Cell (These String I.Int)
_page = _content .. _JTableContent .. JTC._page

_perPage :: TraversalP Cell (Either (These String I.Int) I.Int)
_perPage = _content .. _JTableContent .. JTC._perPage

currentPage :: Cell -> Maybe I.Int
currentPage cell = maybe Nothing theseRight (cell ^? _page)

currentPageSize :: Cell -> Maybe I.Int
currentPageSize cell = maybe Nothing (either theseRight Just) (cell ^? _perPage)

goPage :: forall e. I.Int -> Cell -> (Cell -> I e) -> I e
goPage page cell go = run cell `andThen` \_ -> go (cell # _page .~ That page)

stepPage :: forall e. I.Int -> Cell -> (Cell -> I e) -> I e
stepPage delta cell = goPage (maybe one (delta +) (currentPage cell)) cell

inputPage :: forall e. Cell -> String -> I e
inputPage cell val = update cell (_page .~ thisOrBoth val (currentPage cell))

inputPageSize :: forall e. Cell -> String -> I e
inputPageSize cell val = update cell (_perPage .~ Left (thisOrBoth val $ currentPageSize cell))

loadPage :: forall e. Cell -> (Cell -> I e) -> I e
loadPage cell go = case cell ^? _content .. _JTableContent of
  Nothing -> empty
  Just table ->
    let page = these (readPageNum one) id (flip readPageNum) (table ^. JTC._page)
        perPage = either (these (readPageNum one) id (flip readPageNum)) id (table ^. JTC._perPage)
    in run cell `andThen` \_ -> go (cell # (_page .~ That page)
                                        .. (_perPage .~ Right perPage))
    where
    readPageNum :: I.Int -> String -> I.Int
    readPageNum default str =
      let num = readInt 10 str
      in if isNaN num then default else I.fromNumber num

changePageSize :: forall e. Cell -> (Cell -> I e) -> String -> I e
changePageSize cell go "Custom" = update cell (_perPage .~ Left (That $ fromMaybe Config.defaultPageSize $ currentPageSize cell))
changePageSize cell go value = case readJSON value of
  Left _ -> empty
  Right n -> run cell
    `andThen` \_ -> go (cell # (_perPage .~ Right (I.fromNumber n))
                            .. (_page .~ That one))

runJTable :: forall e. Resource -> Cell -> I e
runJTable file cell = do
  let perPage = fromMaybe Config.defaultPageSize (currentPageSize cell)
      pageNumber = fromMaybe one (currentPage cell)
      pageIndex = pageNumber - one
  results <- liftAff $ attempt $ { numItems: _, json: _ }
    <$> fromMaybe 0 <<< readTotal <$> query file "SELECT COUNT(*) AS total FROM {{path}}"
    <*> sample file (pageIndex * perPage) perPage
  now' <- liftEff now
  return $ case results of
      Left err -> CellResult (cell ^. _cellId) now' (Left $ NEL.singleton $ message err)
      Right results -> do
        CellResult (cell ^. _cellId) now' $ Right $ JTableContent $
          JTC.JTableContent { perPage: Right perPage
                            , page: That pageNumber
                            , result: Just $ JTC.Result
                              { totalPages: I.fromNumber $ Math.ceil (results.numItems / I.toNumber perPage)
                              , values: Just $ fromArray (readValue <$> results.json)
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

queryToJTable :: forall e. Cell -> String -> Resource -> Resource -> I e
queryToJTable cell sql inp out = do
  jobj <- liftAff do
    delete out
    attempt (port inp out sql)
  either errorInQuery (const $ runJTable out cell) jobj
  where
  correct :: String -> Resource -> Resource -> I e
  correct sql inp out = do
    jobj <- liftAff do
      delete out
      attempt (port inp out sql)
    either errorInQuery (const $ runJTable out cell) jobj

  errorInQuery :: _ -> I e
  errorInQuery err =
    update cell (_failures .~ ["Error in query: " <> message err])
      `andThen` \_ -> finish cell
