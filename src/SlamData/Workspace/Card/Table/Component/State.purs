{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Card.Table.Component.State
  ( State
  , ResultR
  , initialState
  , _result
  , _Result
  , _page
  , _pageSize
  , _isEnteringPageSize
  , _size
  , PageInfo
  , currentPageInfo
  , stepPage
  , setPage
  , resizePage
  , setPageSize
  , toModel
  , fromModel
  , calcTotalPages
  ) where

import SlamData.Prelude

import Data.Argonaut (Json)
import Data.Foldable (maximum)
import Data.Int as Int
import Data.Lens (Lens', Traversal', _Just, lens, (?~), (^?))
import SlamData.Workspace.Card.Table.Component.Query (PageStep(..))
import SlamData.Workspace.Card.Table.Model (Model)

-- | The state for the Table card component.
type State =
  { size ∷ Maybe Int
  , result ∷ Maybe ResultR
  , page ∷ Maybe (String ⊹ Int)
  , pageSize ∷ Maybe (Either String Int)
  , isEnteringPageSize ∷ Boolean
  }

-- | The current result value being displayed.
type ResultR =
  { json ∷ Json
  , page ∷ Int
  , pageSize ∷ Int
  }

initialState ∷ State
initialState =
  { size: Nothing
  , result: Nothing
  , page: Nothing
  , pageSize: Nothing
  , isEnteringPageSize: false
  }

_result ∷ ∀ a r. Lens' {result ∷ a|r} a
_result = lens _.result (_ { result = _ })

_Result ∷ Traversal' State ResultR
_Result = _result ∘ _Just

_page ∷ ∀ a r. Lens' {page ∷ a |r} a
_page = lens _.page (_ { page = _ })

_pageSize ∷ ∀ a r. Lens' {pageSize ∷ a|r} a
_pageSize = lens _.pageSize (_ { pageSize = _ })

_size ∷ ∀ a r. Lens' {size ∷ a|r} a
_size = lens _.size _{ size = _ }

-- | Specifies whether the custom page size entry is currently enabled.
_isEnteringPageSize ∷ ∀ a r. Lens' {isEnteringPageSize ∷ a|r} a
_isEnteringPageSize = lens _.isEnteringPageSize (_ { isEnteringPageSize = _ })

-- | A record with information about the current page number, page size, and
-- | total number of pages.
type PageInfo = { page ∷ Int, pageSize ∷ Int, totalPages ∷ Int }

currentPageInfo ∷ State → PageInfo
currentPageInfo st =
  let
    result = st ^? _Result
    page = fromMaybe 1 $ map _.page result
    pageSize = fromMaybe 0 $ map _.pageSize result
    itemCount = fromMaybe 0 $ st ^? _size ∘ _Just
    total = calcTotalPages {pageSize, itemCount}
  in
    { page
    , pageSize
    , totalPages: total
    }

-- | Changes the current page based on a PageStep increment.
stepPage ∷ PageStep → State → State
stepPage step st = st # _page ?~ Right (pageStepValue step st)

-- | Sets the pending custom page value based on text input.
setPage ∷ String → State → State
setPage page = _page ?~ Left page

-- | Takes a `PageStep` and the current pagination settings and produces a new
-- | page number within sensible bounds.
-- |
-- | Only the "actual" values present for the page number and page size are used
-- | to calculate the new page number, any pending values will be ignored.
pageStepValue ∷ PageStep → State → Int
pageStepValue step st = go step
  where
  page =
    fromMaybe 1 $ _.page <$> st ^? _Result

  total =
    totalPages st

  go First = 1
  go Last = total
  go Prev
    | page <= 1 = 1
    | otherwise = page - 1
  go Next
    | page >= total = total
    | otherwise = page + 1

totalPages ∷ State → Int
totalPages st =
  fromMaybe 1
    $ map calcTotalPages
    $ { pageSize: _, itemCount: _}
    <$> (_.pageSize <$> st ^? _Result)
    <*> (st ^? _size ∘ _Just)

-- | Calculates the total number of pages for a given page size and total
-- | number of results.
calcTotalPages ∷ { pageSize ∷ Int, itemCount ∷ Int } → Int
calcTotalPages { pageSize: 0} = 1
calcTotalPages { itemCount: 0} = 1
calcTotalPages { pageSize, itemCount} =
  fromMaybe 1 $ maximum [1, Int.ceil (Int.toNumber itemCount / Int.toNumber pageSize)]

-- | Sets the pending page size based on a concrete numeric value.
resizePage ∷ Int → State → State
resizePage size = _pageSize ?~ Right size

-- | Sets the pending custom page size based on text input.
setPageSize ∷ String → State → State
setPageSize size = _pageSize ?~ Left size

toModel ∷ State → Model
toModel st =
  { page: either Int.fromString Just =<< st.page
  , pageSize: either Int.fromString Just =<< st.pageSize
  }

fromModel ∷ Model → State
fromModel m =
  initialState
    { page = Right <$> m.page
    , pageSize = Right <$> m.pageSize
    }
