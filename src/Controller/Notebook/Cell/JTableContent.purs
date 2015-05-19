module Controller.Notebook.Cell.JTableContent
  ( goPage
  , stepPage
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
import Controller.Notebook.Common (I())
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (Json(), JObject(), fromArray, toObject, toNumber, fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (head)
import Data.Date (now, nowEpochMilliseconds, toEpochMilliseconds)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Foreign.Class (readJSON)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time (Milliseconds())
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(..), CellResultContent(..))
import Model.Notebook.Cell
import Model.Resource (Resource())
import Optic.Core ((^.), (.~), (..))
import Optic.Extended (TraversalP(), (^?))

import qualified Model.Notebook.Cell.JTableContent as JTC
import qualified Data.Int as I
import qualified Data.StrMap as SM

goPage :: forall e. I.Int -> Cell -> (Cell -> I e) -> I e
goPage page cell run =
  ((RunCell (cell ^. _cellId)) <$> liftEff now) `andThen`
    \_ -> run (cell # _content .. _JTableContent .. JTC._page .~ page)

stepPage :: forall e. I.Int -> Cell -> (Cell -> I e) -> I e
stepPage delta cell run =
  let page :: I.Int
      page = maybe (I.fromNumber 1) (delta +) (cell ^? _content .. _JTableContent .. JTC._page)
  in ((RunCell (cell ^. _cellId)) <$> liftEff now) `andThen`
    \_ -> run (cell # _content .. _JTableContent .. JTC._page .~ page)

changePageSize :: forall e. Cell -> (Cell -> I e) -> String -> I e
changePageSize cell run value = case readJSON value of
  Left _ -> empty
  Right n ->
    ((RunCell (cell ^. _cellId)) <$> liftEff now) `andThen`
      \_ -> run (cell # (_content .. _JTableContent .. JTC._perPage .~ I.fromNumber n)
                     .. (_content .. _JTableContent .. JTC._page .~ one))

runJTable :: forall e. Resource -> Cell -> I e
runJTable file cell = fromMaybe empty $ do
  table <- cell ^? _content .. _JTableContent
  return $ do
    let perPage = table ^. JTC._perPage
        pageNumber = table ^. JTC._page
        pageIndex = pageNumber - I.fromNumber 1
    -- TODO: catch aff failures?
    numItems <- liftAff $ fromMaybe 0 <<< readTotal <$> query file "SELECT COUNT(*) AS total FROM {{path}}"
    result <- liftAff $ sample file (pageIndex * perPage) perPage
    now' <- liftEff now
    return $ CellResult (cell ^. _cellId) now' $ Right $ JTableContent $
      JTC.JTableContent { perPage: perPage
                        , page: pageNumber
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

queryToJTable :: forall e. Cell -> String -> Resource -> Resource -> I e
queryToJTable cell sql inp out = do
  jobj <- liftAff do
    delete out
    attempt (port inp out sql)
  either errorInQuery (const $ runJTable out cell) jobj
  where
  started :: Maybe Milliseconds
  started = toEpochMilliseconds <$> (cell ^? _runState .. _RunningSince)

  update :: (Cell -> Cell) -> Input
  update = UpdateCell (cell ^. _cellId)

  correct :: String -> Resource -> Resource -> I e
  correct sql inp out = do
    jobj <- liftAff do
      delete out
      attempt (port inp out sql)
    either errorInQuery (const $ runJTable out cell) jobj

  errorInQuery :: _ -> I e
  errorInQuery err =
    (pure $ update (_failures .~ ["Error in query: " <> message err]))
    `andThen` \_ -> finish

  finish :: I e
  finish = do
    d <- liftEff nowEpochMilliseconds
    pure $ update (_runState .~ RunFinished (maybe zero (d -) started))
