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

-- | Describe model of html `select` elements and in general all kind of
-- | selectable stuff.
module SlamData.Form.Select where

import SlamData.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JCursor, decodeJson, jsonEmptyObject, (.?), (~>), (:=))
import Data.Array as Arr
import Data.Array (filter, length, head, (!!), elemIndex)
import Data.Lens (Lens', lens, view, (^.), (?~), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Monoid.Conj (Conj(..))
import Data.Set as Set
import Test.StrongCheck.Arbitrary as SC

class (Eq a) ⇐ OptionVal a where
  stringVal ∷ a → String

instance optionValJCursor ∷ OptionVal JCursor where
  stringVal = show

instance optionValMaybe ∷ (OptionVal a) ⇒ OptionVal (Maybe a) where
  stringVal Nothing = "None"
  stringVal (Just a) = stringVal a

-- | `options` for available variants
-- | `value` for selected item
type SelectR a =
  { options ∷ Array a
  , value ∷ Maybe a
  }

newtype Select a = Select { options ∷ Array a, value ∷ Maybe a }

derive instance newtypeSelect ∷ Newtype (Select a) _
derive instance eqSelect ∷ Eq a ⇒ Eq (Select a)
derive instance functorSelect ∷ Functor Select

instance encodeJsonSelect ∷ (EncodeJson a) ⇒ EncodeJson (Select a) where
  encodeJson (Select r) =
    "options" := r.options ~> "value" := r.value ~> jsonEmptyObject

instance decodeJsonSelect ∷ (DecodeJson a) ⇒ DecodeJson (Select a) where
  decodeJson json = do
    obj ← decodeJson json
    r ← { options: _ , value: _ } <$> (obj .? "options") <*> (obj .? "value")
    pure $ Select r

instance showSelect ∷ (Show a) ⇒ Show (Select a) where
  show (Select s) =
    "(Select {options = " <> show s.options <> ", value = " <> show s.value <> "})"

instance arbitrarySelect ∷ (SC.Arbitrary a) ⇒ SC.Arbitrary (Select a) where
  arbitrary = do
    value ← SC.arbitrary
    options ← SC.arbitrary
    pure $ Select { value, options }


_options ∷ ∀ a. Lens' (Select a) (Array a)
_options = _Newtype ∘ lens _.options _{options = _}

_value ∷ ∀ a. Lens' (Select a) (Maybe a)
_value = _Newtype ∘ lens _.value _{value = _}

emptySelect ∷ ∀ a. Select a
emptySelect = Select { options: [ ], value: Nothing}

newSelect ∷ ∀ a f. Foldable f ⇒ f a → Select a
newSelect as = Select { options: Arr.fromFoldable as, value: Nothing }

fromSelected ∷ ∀ a. Maybe a → Select a
fromSelected = case _ of
  Nothing → Select { options: [], value: Nothing }
  Just a → Select { options: [a], value: Just a}

-- | Take first select and remove all from its options second select's value
except ∷ ∀ a. (Eq a) ⇒ Select a → Select a → Select a
except (Select r) subtrahend@(Select rr) =
  Select
    { options: except' subtrahend r.options
    , value: value
    }
  where
  value ∷ Maybe a
  value = if r.value == rr.value
          then Nothing
          else r.value

-- | Filter array to exclude value of `Select`
except' ∷ ∀ a. (Eq a) ⇒ Select a → Array a → Array a
except' sel arr = filter (\x → Just x /= (sel ^. _value)) arr

-- | If there is only one option (opt) set value to be `Just opt`
autoSelect ∷ ∀ a. (Eq a) ⇒ Select a → Select a
autoSelect (Select {options: opts, value: val}) =
  if length opts == 1
  then Select {options: opts, value: head opts}
  else Select {options: opts, value: val}

trySelect ∷ ∀ a. (Eq a) ⇒ Int → Select a → Select a
trySelect i sel =
  maybe (sel # _value .~ Nothing) (\v → sel # _value ?~ v) (sel ^. _options !! i)

trySelect' ∷ ∀ a. (Eq a) ⇒ a → Select a → Select a
trySelect' a sel =
  maybe sel (\_ → sel # _value ?~ a) $ elemIndex a $ sel ^. _options

-- | Flipped version of `except'` useful for filtering model fields by
-- | view values i.e.
-- | ```purescript
-- | type State = { users ∷ Array User }
-- | type ViewModel = { userSelect ∷ Select User }
-- |
-- | updateState ∷ ViewModel → State → State
-- | updateState vm st = st { users = st.users ⊝ vm.userSelect }
-- | ```
infixl 2 exceptFlipped as ⊝

exceptFlipped ∷ ∀ a. Ord a ⇒ Set.Set a → Select a → Set.Set a
exceptFlipped st (Select { value }) =
  maybe st (flip Set.delete st) value

ifSelected
  ∷ ∀ f m a b
  . (Foldable f, Monoid (m b))
  ⇒ f (Select a)
  → m b
  → m b
ifSelected sels arr =
  if alaF Conj foldMap (isJust ∘ view _value) sels
    then arr
    else mempty

setPreviousValueFrom
  ∷ ∀ a. (Eq a) ⇒ Maybe (Select a) → Select a → Select a
setPreviousValueFrom mbSel target  =
  (maybe id trySelect' $ mbSel >>= view _value) $ target
