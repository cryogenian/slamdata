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

module Data.Selection where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (filter, length, head)
import Data.Argonaut
import Optic.Core


type SelectionR a =
  { variants :: Array a
  , selection :: Maybe a
  }

newtype Selection a = Selection (SelectionR a)


_Selection :: forall a. LensP (Selection a) (SelectionR a)
_Selection = lens (\(Selection obj) -> obj) (const Selection)

_variants :: forall a. LensP (Selection a) (Array a)
_variants = _Selection <<< lens _.variants _{variants = _}

_selR :: forall a. LensP (SelectionR a) (Maybe a)
_selR = lens _.selection _{selection = _}

_selection :: forall a. LensP (Selection a) (Maybe a)
_selection = _Selection <<< _selR

initialSelection :: forall a. Selection a
initialSelection = Selection {variants: [], selection: Nothing}

newSelection :: forall a. Array a -> Selection a
newSelection as = Selection {variants: as, selection: Nothing}

except :: forall a. (Eq a) => Selection a -> Selection a -> Selection a
except sel sel' = sel # (_variants %~ (flip except' sel'))

except' :: forall a. (Eq a) => Array a -> Selection a -> Array a
except' lst sel = filter (\x -> Just x /= (sel ^._selection)) lst

autoSelect :: forall a. (Eq a) => Selection a -> Selection a
autoSelect initial@(Selection {variants: arr, selection: sel}) =
  if length arr == 1
  then Selection {variants: arr, selection: head arr}
  else initial


infix 9 <->
(<->) = except'

instance encodeJsonSelection :: (EncodeJson a) => EncodeJson (Selection a) where
  encodeJson (Selection r) = "variants" := r.variants
                             ~> "selection" := r.selection
                             ~> jsonEmptyObject

instance decodeJsonSelection :: (DecodeJson a) => DecodeJson (Selection a) where
  decodeJson json = do
    obj <- decodeJson json
    r <- { variants: _
         , selection: _
         } <$>
         (obj .? "variants") <*>
         (obj .? "selection")
    pure $ Selection r


