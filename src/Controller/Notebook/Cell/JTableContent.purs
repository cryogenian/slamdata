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

import Prelude
import qualified Api.Fs as Quasar
import qualified Api.Query as Quasar
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Plus (empty)
import Controller.Notebook.Common (I(), run, update, finish)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (Json(), JObject(), fromArray)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (head, last)
import Data.Bifunctor (lmap, rmap)
import Data.Functor (($>))
import Data.Date (now)
import Data.Either (Either(..), either)
import Data.Foreign.Class (readJSON)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.These (These(..), these, theseRight, thisOrBoth)
import Global (isNaN, readInt)
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(..), CellResultContent(..))
import Model.Notebook.Cell
import Model.Notebook.Port (VarMapValue(), _VarMap, _PortResource, Port(..))
import Model.Resource (Resource(..), mkFile, isTempFile, _path)
import Optic.Core
import Optic.Extended (TraversalP(), (^?))
import Utils (s2i)

import Data.Path.Pathy ((</>), parseAbsFile, rootDir, sandbox)

import qualified Data.Array.NonEmpty as NEL
import qualified Data.Int as I
import qualified Data.StrMap as SM
import qualified Model.Notebook.Cell.JTableContent as JTC

import qualified Data.StrMap as SM

_page :: TraversalP Cell (These String Int)
_page = _content .. _JTableContent .. JTC._page

_perPage :: TraversalP Cell (Either (These String Int) Int)
_perPage = _content .. _JTableContent .. JTC._perPage

currentPage :: Cell -> Maybe Int
currentPage cell = maybe Nothing theseRight (cell ^? _page)

currentPageSize :: Cell -> Maybe Int
currentPageSize cell = maybe Nothing (either theseRight Just) (cell ^? _perPage)

goPage :: forall e. Int -> Cell -> (Cell -> I e) -> I e
goPage page cell go = run cell `andThen` \_ -> go (cell # _page .~ That page)

stepPage :: forall e. Int -> Cell -> (Cell -> I e) -> I e
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
    readPageNum :: Int -> String -> Int
    readPageNum default str = fromMaybe default $ s2i str


changePageSize :: forall e. Cell -> (Cell -> I e) -> String -> I e
changePageSize cell go "Custom" = update cell (_perPage .~ Left (That $ fromMaybe Config.defaultPageSize $ currentPageSize cell))
changePageSize cell go value = case readJSON value of
  Left _ -> empty
  Right n -> run cell
    `andThen` \_ -> go (cell # (_perPage .~ Right n)
                            .. (_page .~ That one))

runJTable :: forall e. Resource -> Cell -> I e
runJTable file cell = do
  let perPage = fromMaybe Config.defaultPageSize (currentPageSize cell)
      pageNumber = fromMaybe one (currentPage cell)
      pageIndex = pageNumber - one
  results <- liftAff $ attempt $ do
    numItems <- Quasar.countWithQuery file
    let numPages = Math.ceil (I.toNumber numItems / I.toNumber perPage)
        pageIndex' = fromMaybe 0 $ I.fromNumber $ Math.max 0.0 $ Math.min (I.toNumber pageIndex) (numPages - 1.0)
    json <- Quasar.sample file (Just $ pageIndex' * perPage) (Just perPage)
    return { numItems: numItems, pageNumber: pageIndex' + one, json: json }
  now' <- liftEff now
  return $ case results of
      Left err -> CellResult (cell ^. _cellId) now' (Left $ NEL.singleton $ message err)
      Right results -> do
        CellResult (cell ^. _cellId) now' $ Right $ JTableContent $
          JTC.JTableContent { perPage: Right perPage
                            , page: That results.pageNumber
                            , result: Just $ JTC.Result
                              { totalPages: fromMaybe 0 $ I.fromNumber $
                                Math.ceil (I.toNumber results.numItems / I.toNumber perPage)
                              , values: Just $ fromArray results.json
                              }
                            }

-- | Executes a cell's query on Quasar; if the cell's VarMap is empty, then
-- | it will be done using the view mounts API.
-- |
-- | TODO: Remove String from being our error type.
queryToJTable :: forall e. Cell -> String -> Resource -> Resource -> I e
queryToJTable cell sql inp out = do
  jobj <- liftAff do
    -- TODO: forceDelete doesn't work on views, but we have no way of knowing whether
    -- the file is a view or not at this time. So, for now, we have to just try it.
    void $ attempt $
      if isTempFile out
         then Quasar.forceDelete out
         else pure unit

    -- If result-caching is enabled, use the query API rather than the views API
    let shouldCacheResults = cell ^? _content .. _shouldCacheResults # fromMaybe false
    attempt $
      if SM.isEmpty varMap && not shouldCacheResults
         then Quasar.portView inp out sql $> Nothing
         else Quasar.portQuery inp out sql varMap <#> Just

  either errorInQuery go do
    mj <- lmap message jobj
    case mj of
      Nothing -> do
        path <-
          case out of
            File p -> pure p
            ViewMount p -> pure p
            _ -> Left "Expected out as file or view mount"
        realOut <- maybe (Left "Could not sandbox Quasar file") Right $ sandbox rootDir path
        pure { realOut: realOut, plan: "" }
      Just j -> do
        out' <- j .? "out"
        planPhases <- last <$> j .? "phases"
        let plan = maybe "" (\p -> either (const "") id $ p .? "detail") planPhases
        path <- maybe (Left "Invalid file from Quasar") Right $ parseAbsFile out'
        realOut <- maybe (Left "Could not sandbox Quasar file") Right $ sandbox rootDir path
        pure { realOut: realOut, plan: plan }
  where
  go { realOut: realOut, plan: plan } =
    let file = mkFile (Left $ rootDir </> realOut) in
    (update cell $ (_output .~ PortResource file) .. (_message .~ plan)) <>
    (pure $ UpdatedOutput (cell ^._cellId) (PortResource file)) <>
    (runJTable file cell)

  varMap :: SM.StrMap VarMapValue
  varMap = fromMaybe SM.empty $ cell ^? _input.._VarMap

  errorInQuery :: _ -> I e
  errorInQuery err =
    update cell (_failures .~ ["Error in query: " <> err])
      `andThen` \_ -> finish cell

