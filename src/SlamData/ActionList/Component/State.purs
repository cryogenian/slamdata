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

module SlamData.ActionList.Component.State where

import SlamData.Prelude

import Data.Lens (Lens', lens)

import DOM.HTML.Types (HTMLElement)

import SlamData.ActionList.Component.ActionInternal (ActionInternal, Dimensions)

type State a =
  { actions ∷ Array (ActionInternal a)
  , previousActions ∷ Array (ActionInternal a)
  , filterString ∷ String
  , boundingElement ∷ Maybe HTMLElement
  , boundingDimensions ∷ Maybe Dimensions
  }

_actions ∷ ∀ a r. Lens' { actions ∷ a |r } a
_actions =
  lens _.actions (_ { actions = _ })

_previousActions ∷ ∀ a r. Lens' { previousActions ∷ a | r} a
_previousActions =
  lens _.previousActions (_ { previousActions = _ })

_filterString ∷ ∀ a r. Lens' { filterString ∷ a | r } a
_filterString =
  lens _.filterString (_ { filterString = _ })

_boundingElement ∷ ∀ a r. Lens' { boundingElement ∷ a | r } a
_boundingElement =
  lens _.boundingElement (_ { boundingElement = _ })

_boundingDimensions ∷ ∀ a r. Lens' { boundingDimensions ∷ a | r } a
_boundingDimensions =
  lens _.boundingDimensions (_ { boundingDimensions = _ })
