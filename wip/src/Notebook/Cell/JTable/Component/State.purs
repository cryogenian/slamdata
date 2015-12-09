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

module Notebook.Cell.JTable.Component.State
  ( JTableState()
  , JTableInput()
  , initialJTableState
  , _input
  , _json
  , _page
  , _pageSize
  , _isEnteringPageSize
  , _resource
  , _size
  , PageInfo()
  , currentPageInfo
  , pendingPageInfo
  , stepPage
  , setPage
  , resizePage
  , setPageSize
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))

import Data.Argonaut.Core (Json())
import Data.Either (Either(..), either)
import Data.Foldable (maximum)
import Data.Int as Int
import Data.Lens ((^?), (%~), LensP(), lens, _Just)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.These (These(..), theseLeft, theseRight)

import Model.Resource (Resource())

import Notebook.Cell.JTable.Component.Query (PageStep(..))

-- | The state for the JTable cell component.
type JTableState =
  { input :: Maybe JTableInput
  , json :: Maybe Json
  , page :: These (Either String Int) Int
  , pageSize :: These (Either String Int) Int
  , isEnteringPageSize :: Boolean
  }

-- | The cell input state.
type JTableInput =
  { resource :: Resource
  , size :: Int
  }

-- | An empty initial state for the JTable cell component.
initialJTableState :: JTableState
initialJTableState =
  { input: Nothing
  , json: Nothing
  , page: This (Right 1)
  , pageSize: This (Right 10)
  , isEnteringPageSize: false
  }

-- | The current cell input - a resource location for the results, and the size
-- | of the result set.
_input :: LensP JTableState (Maybe JTableInput)
_input = lens _.input (_ { input = _ })

-- | The currently loaded portion of the result set.
_json :: LensP JTableState (Maybe Json)
_json = lens _.json (_ { json = _ })

-- | The requested and current page number - the right side holds the page
-- | number for the current result set, the left side holds a page number that
-- | has not yet been submitted for loading, with a possibility for a
-- | user-entered arbitrary value that has not yet been parsed.
_page :: LensP JTableState (These (Either String Int) Int)
_page = lens _.page (_ { page = _ })

-- | The request and current page size - the right side holds the page size
-- | for the current result set, the left side holds a page size that
-- | has not yet been submitted for loading, with a possibility for a
-- | user-entered arbitrary value that has not yet been parsed.
_pageSize :: LensP JTableState (These (Either String Int) Int)
_pageSize = lens _.pageSize (_ { pageSize = _ })

-- | Specifies whether the custom page size entry is currently enabled.
_isEnteringPageSize :: LensP JTableState Boolean
_isEnteringPageSize = lens _.isEnteringPageSize (_ { isEnteringPageSize = _ })

-- | The resource to load pages of data from.
_resource :: LensP JTableInput Resource
_resource = lens _.resource (_ { resource = _ })

-- | The total size of the resource's result set.
_size :: LensP JTableInput Int
_size = lens _.size (_ { size = _ })

-- | A record with information about the current page number, page size, and
-- | total number of pages.
type PageInfo = { page :: Int, pageSize :: Int, totalPages :: Int }

currentPageInfo :: JTableState -> PageInfo
currentPageInfo st =
  let page = fromMaybe 1 $ theseRight st.page
      pageSize = fromMaybe 0 $ theseRight st.pageSize
      total = fromMaybe 0 $ st ^? _input <<< _Just <<< _size
      totalPages = calcTotalPages pageSize total
  in { page, pageSize, totalPages }

-- | Compute page info from the current state, preferring user-entered pending
-- | values if they are present and succesfully parseable.
pendingPageInfo :: JTableState -> PageInfo
pendingPageInfo st =
  let customPage = either Int.fromString pure =<< theseLeft st.page
      actualPage = theseRight st.page
      page = fromMaybe 1 $ customPage <|> actualPage
      customPageSize = either Int.fromString pure =<< theseLeft st.pageSize
      actualPageSize = theseRight st.pageSize
      pageSize = fromMaybe 0 $ customPageSize <|> actualPageSize
      total = fromMaybe 0 $ st ^? _input <<< _Just <<< _size
      totalPages = calcTotalPages pageSize total
      page' = if page > totalPages then totalPages else page
  in { page: page', pageSize, totalPages }

-- | Changes the current page based on a PageStep increment.
stepPage :: PageStep -> JTableState -> JTableState
stepPage step st = st # _page %~ setTheseLeft (Right (pageStepValue step st))

-- | Sets the pending custom page value based on text input.
setPage :: String -> JTableState -> JTableState
setPage page = _page %~ setTheseLeft (Left page)

-- | Takes a `PageStep` and the current pagination settings and produces a new
-- | page number within sensible bounds.
-- |
-- | Only the "actual" values present for the page number and page size are used
-- | to calculate the new page number, any pending values will be ignored.
pageStepValue :: PageStep -> JTableState -> Int
pageStepValue step st = go step
  where
  page = fromMaybe 1 (theseRight st.page)
  totalPages = fromMaybe 1 $ calcTotalPages <$> theseRight st.pageSize <*> (st ^? _input <<< _Just <<< _size)
  go First = 1
  go Last = totalPages
  go Prev
    | page <= 1 = 1
    | otherwise = page - 1
  go Next
    | page >= totalPages = totalPages
    | otherwise = page + 1

-- | Calculates the total number of pages for a given page size and total
-- | number of results.
calcTotalPages :: Int -> Int -> Int
calcTotalPages _ 0 = 1
calcTotalPages 0 _ = 1
calcTotalPages pageSize total =
  fromMaybe 1 $ maximum [1, Int.ceil (Int.toNumber total / Int.toNumber pageSize)]

-- | Sets the pending page size based on a concrete numeric value.
resizePage :: Int -> JTableState -> JTableState
resizePage size = _pageSize %~ setTheseLeft (Right size)

-- | Sets the pending custom page size based on text input.
setPageSize :: String -> JTableState -> JTableState
setPageSize size = _pageSize %~ setTheseLeft (Left size)

-- | A utility function for setting the left component of a `These` while
-- | preserving any existing right component value.
setTheseLeft :: forall a b. a -> These a b -> These a b
setTheseLeft a (This _) = This a
setTheseLeft a (Both _ b) = Both a b
setTheseLeft a (That b) = Both a b
