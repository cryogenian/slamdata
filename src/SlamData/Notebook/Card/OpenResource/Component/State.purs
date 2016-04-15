module SlamData.Notebook.Card.OpenResource.Component.State where

import SlamData.Prelude

import Data.Lens (LensP, lens)

import SlamData.FileSystem.Resource as R

import Utils.Path as Up

import Data.Path.Pathy
import Data.Maybe.Unsafe

type State =
  { items ∷ Array R.Resource
  , browsing ∷ Up.DirPath
  , selected ∷ Maybe R.Resource
  }

initialState ∷ State
initialState =
  { items: [ ]
  , browsing: Unsafe.Coerce.unsafeCoerce $ fromJust $ parseAbsDir "/"
  , selected: Nothing
  }


_items ∷ ∀ a r. LensP { items ∷ a |r} a
_items = lens (_.items) (_{items = _})

_browsing ∷ ∀ a r. LensP { browsing ∷ a |r} a
_browsing = lens (_.browsing) (_{browsing = _})

_selected ∷ ∀ a r. LensP { selected ∷ a|r} a
_selected = lens (_.selected) (_{selected = _})
