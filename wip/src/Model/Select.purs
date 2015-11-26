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

import Control.Apply ((*>))
import Control.MonadPlus (MonadPlus, guard)
import Data.Argonaut
  (DecodeJson, EncodeJson, JCursor(), decodeJson, jsonEmptyObject, (.?), (~>), (:=))
import Data.Array (filter, length, head, (!!), elemIndex)
import Data.Foldable (Foldable, foldMap)
import Data.Lens (LensP(), lens, view, (^.), (?~), (.~))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Monoid.Conj (Conj(..), runConj)


class (Eq a) <= OptionVal a where
  stringVal :: a -> String

instance optionValJCursor :: OptionVal JCursor where
  stringVal = show

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
except (Select r) subtrahend@(Select rr) =
  Select { options: except' subtrahend r.options
         , value: value
         }
  where
  value :: Maybe a
  value = if r.value == rr.value
          then Nothing
          else r.value

-- | Filter array to exclude value of `Select`
except' :: forall a. (Eq a) => Select a -> Array a -> Array a
except' sel arr = filter (\x -> Just x /= (sel ^. _value)) arr

filterSelect :: forall a. (Eq a) => Select a -> a -> Select a
filterSelect (Select r) val =
  Select { options: filter (/= val) r.options
         , value: if Just val == r.value then Nothing else r.value
         }
-- This function returns `Select` without provided value if it is
-- `Just a` or `emptySelect` if second argument is `Nothing`
-- This is useful in case we want to make two connected selects
-- where dependent select is disabled when main has no value.
filterSelect' :: forall a. (Eq a) => Select a -> Maybe a -> Select a
filterSelect' sel val =
  maybe emptySelect (filterSelect sel) val

-- | If there is only one option (opt) set value to be `Just opt`
autoSelect :: forall a. (Eq a) => Select a -> Select a
autoSelect (Select {options: opts, value: val}) =
  if length opts == 1
  then Select {options: opts, value: head opts}
  else Select {options: opts, value: val}

trySelect :: forall a. (Eq a) => Int -> Select a -> Select a
trySelect i sel =
  maybe (sel # _value .~ Nothing) (\v -> sel # _value ?~ v) (sel ^. _options !! i)

trySelect' :: forall a. (Eq a) => a -> Select a -> Select a
trySelect' a sel =
  maybe sel (\_ -> sel # _value ?~ a) $ elemIndex a $ sel ^. _options

-- | Flipped version of `except'` useful for filtering model fields by
-- | view values i.e.
-- | ```purescript
-- | type State = { users :: Array User }
-- | type ViewModel = { userSelect :: Select User }
-- |
-- | updateState :: ViewModel -> State -> State
-- | updateState vm st = st { users = st.users <-> vm.userSelect }
-- | ```
infixl 2 <->
(<->) :: forall a. (Eq a) => Array a -> Select a -> Array a
(<->) = flip except'

ifSelected
  :: forall f m a. (Foldable f, MonadPlus m)
  => f (Select a) -> m a -> m a
ifSelected sels arr =
  guard (runConj $ foldMap (Conj <<< isJust <<< view _value) sels) *> arr

instance eqSelect :: (Eq a) => Eq (Select a) where
  eq (Select r) (Select rr) =
       r.value == rr.value
    && r.options == rr.options

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
