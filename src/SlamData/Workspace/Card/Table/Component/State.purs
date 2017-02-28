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
  , Result(..)
  , ResultR
  , Input
  , initialState
  , _input
  , _result
  , _Result
  , _page
  , _pageSize
  , _isEnteringPageSize
  , _resource
  , _size
  , PageInfo
  , currentPageInfo
  , pendingPageInfo
  , stepPage
  , setPage
  , resizePage
  , setPageSize
  , toModel
  , fromModel
  ) where

import SlamData.Prelude

import Data.Argonaut (Json)
import Data.Foldable (maximum)
import Data.Int as Int
import Data.Lens ((^?), (?~), Lens', lens, _Just, Prism', prism', Traversal')

import SlamData.Workspace.Card.Table.Component.Query (PageStep(..))
import SlamData.Workspace.Card.Table.Model (Model)
import SlamData.Workspace.Card.Port as Port

-- | The state for the Table card component.
type State =
  { input ∷ Maybe Input
  , result ∷ Result
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

data Result
  = Loading
  | Errored String
  | Empty
  | Ready ResultR

_ResultR ∷ Prism' Result ResultR
_ResultR = prism' Ready case _ of
  Ready r → Just r
  _ → Nothing

-- | An empty initial state for the Table card component.
initialState ∷ State
initialState =
  { input: Nothing
  , result: Loading
  , page: Nothing
  , pageSize: Nothing
  , isEnteringPageSize: false
  }

-- | The current card input - a resource location for the results, and the size
-- | of the result set.
_input ∷ ∀ a r. Lens' {input ∷ a|r} a
_input = lens _.input (_ { input = _ })

_result ∷ ∀ a r. Lens' {result ∷ a|r} a
_result = lens _.result (_ { result = _ })

_Result ∷ Traversal' State ResultR
_Result = _result ∘ _ResultR

_page ∷ ∀ a r. Lens' {page ∷ a |r} a
_page = lens _.page (_ { page = _ })

_pageSize ∷ ∀ a r. Lens' {pageSize ∷ a|r} a
_pageSize = lens _.pageSize (_ { pageSize = _ })

-- | Specifies whether the custom page size entry is currently enabled.
_isEnteringPageSize ∷ ∀ a r. Lens' {isEnteringPageSize ∷ a|r} a
_isEnteringPageSize = lens _.isEnteringPageSize (_ { isEnteringPageSize = _ })

-- | The card input state.
type Input =
  { resource ∷ Port.Resource
  , size ∷ Int
  }

-- | The resource to load pages of data from.
_resource ∷ ∀ a r. Lens' {resource ∷ a|r} a
_resource = lens _.resource (_ { resource = _ })

-- | The total size of the resource's result set.
_size ∷ ∀ a r. Lens' {size ∷ a|r} a
_size = lens _.size (_ { size = _ })

-- | A record with information about the current page number, page size, and
-- | total number of pages.
type PageInfo = { page ∷ Int, pageSize ∷ Int, totalPages ∷ Int }

currentPageInfo ∷ State → PageInfo
currentPageInfo st =
  let page = fromMaybe 1 $ _.page <$> st ^? _Result
      pageSize = fromMaybe 0 $ _.pageSize <$> st ^? _Result
      total = fromMaybe 0 $ st ^? _input ∘ _Just ∘ _size
      totalPages = calcTotalPages pageSize total
  in { page, pageSize, totalPages }

-- | Compute page info from the current state, preferring user-entered pending
-- | values if they are present and succesfully parseable.
pendingPageInfo ∷ State → PageInfo
pendingPageInfo st =
  let customPage = either Int.fromString pure =<< st.page
      actualPage = _.page <$> st ^? _Result
      page = fromMaybe 1 $ customPage <|> actualPage
      customPageSize = either Int.fromString pure =<< st.pageSize
      actualPageSize = _.pageSize <$> st ^? _Result
      pageSize = fromMaybe 10 $ customPageSize <|> actualPageSize
      total = fromMaybe 0 $ st ^? _input ∘ _Just ∘ _size
      totalPages = calcTotalPages pageSize total
      page' = min page totalPages
  in { page: page', pageSize, totalPages }

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

  totalPages =
    fromMaybe 1
      $ calcTotalPages
      <$> (_.pageSize <$> st ^? _Result)
      <*> (st ^? _input ∘ _Just ∘ _size)

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
calcTotalPages ∷ Int → Int → Int
calcTotalPages _ 0 = 1
calcTotalPages 0 _ = 1
calcTotalPages pageSize total =
  fromMaybe 1 $ maximum [1, Int.ceil (Int.toNumber total / Int.toNumber pageSize)]

-- | Sets the pending page size based on a concrete numeric value.
resizePage ∷ Int → State → State
resizePage size = _pageSize ?~ Right size

-- | Sets the pending custom page size based on text input.
setPageSize ∷ String → State → State
setPageSize size = _pageSize ?~ Left size

toModel ∷ State → Model
toModel st =
  { page: (_.page <$> st ^? _Result) <|> (either (const Nothing) Just =<< st.page)
  , pageSize: (_.pageSize <$> st ^? _Result) <|> (either (const Nothing) Just =<< st.pageSize)
  }

fromModel ∷ Model → State
fromModel m =
  initialState
    { page = Right <$> m.page
    , pageSize = Right <$> m.pageSize
    }
