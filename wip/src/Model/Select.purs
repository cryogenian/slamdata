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

-- | Describe model of html `select` elements and in general all kind of
-- | selectable stuff.
module Model.Select where

import Prelude

import Data.Argonaut
  (DecodeJson, EncodeJson, decodeJson, jsonEmptyObject, (.?), (~>), (:=))
import Data.Array (filter, length, head)
import Data.Lens (LensP(), lens, (%~), (^.))
import Data.Maybe (Maybe(..))

-- | `options` for available variants
-- | `value` for selected item
type SelectR a =
  { options :: Array a
  , value :: Maybe a
  }

newtype Select a = Select (SelectR a)

_Select :: forall a. LensP (Select a) (SelectR a)
_Select = lens (\(Select obj) -> obj) (const Select)

_options :: forall a. LensP (Select a) (Array a)
_options = _Select <<< lens _.options _{options = _}

_value :: forall a. LensP (Select a) (Maybe a)
_value = _Select <<< lens _.value _{value = _}

emptySelect :: forall a. Select a
emptySelect = Select { options: [ ], value: Nothing}

newSelect :: forall a. Array a -> Select a
newSelect as = Select { options: as, value: Nothing }

-- | Take first select and remove all from its options second select's value
except :: forall a. (Eq a) => Select a -> Select a -> Select a
except minuent subtrahend = minuent # (_options %~ except' subtrahend)

-- | Filter array to exclude value of `Select`
except' :: forall a. (Eq a) => Select a -> Array a -> Array a
except' sel arr = filter (\x -> Just x /= (sel ^. _value)) arr

-- | If there is only one option (opt) set value to be `Just opt`
autoSelect :: forall a. (Eq a) => Select a -> Select a
autoSelect (Select {options: opts, value: val}) =
  if length opts == 1
  then Select {options: opts, value: head opts}
  else Select {options: opts, value: val}

infix 9 <->
-- | Flipped version of `except'` useful for filtering model fields by
-- | view values i.e.
-- | ```purescript
-- | type State = { users :: Array User }
-- | type ViewModel = { userSelect :: Select User }
-- |
-- | updateState :: ViewModel -> State -> State
-- | updateState vm st = st { users = st.users <-> vm.userSelect }
-- | ```
(<->) :: forall a. (Eq a) => Array a -> Select a -> Array a
(<->) = flip except'


instance encodeJsonSelect :: (EncodeJson a) => EncodeJson (Select a) where
  encodeJson (Select r) =
    "options" := r.options ~> "value" := r.value ~> jsonEmptyObject

instance decodeJsonSelect :: (DecodeJson a) => DecodeJson (Select a) where
  decodeJson json = do
    obj <- decodeJson json
    r <- { options: _
         , value: _ }
         <$> (obj .? "options")
         <*> (obj .? "value")
    pure $ Select r
