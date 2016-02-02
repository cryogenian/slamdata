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

module SlamData.Notebook.FormBuilder.Component.State
  ( State()
  , ItemId()
  , emptyState
  , initialState

  , addItem
  , removeItem
  ) where

import Prelude

import Data.List as L

type ItemId = Int

type State =
  { items :: L.List ItemId
  , nextId :: ItemId
  }


emptyState :: State
emptyState =
  { items : L.Nil
  , nextId : 0
  }

initialState :: State
initialState = addItem emptyState

addItem
  :: State
  -> State
addItem st =
  st { nextId = st.nextId + 1
     , items = st.items `L.snoc` st.nextId
     }

removeItem
  :: ItemId
  -> State
  -> State
removeItem i st =
  st { items = L.filter (/= i) st.items }

