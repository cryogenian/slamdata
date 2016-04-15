module SlamData.Notebook.Card.OpenResource.Component.State where

import SlamData.Prelude

import SlamData.FileSystem.Resource as R

import Utils.Path as Up

import Data.Path.Pathy
import Data.Maybe.Unsafe

type State =
  { items ∷ Array R.Resource
  , browsing ∷ Up.DirPath
  }

initialState ∷ State
initialState =
  { items:
       [ R.File (Unsafe.Coerce.unsafeCoerce $ fromJust $ parseAbsFile "/foo/bar/baz")
       , R.Directory (Unsafe.Coerce.unsafeCoerce $ fromJust $ parseAbsDir "/foo/bar/bar/")
       ]
  , browsing: Unsafe.Coerce.unsafeCoerce $ fromJust $ parseAbsDir "/lolo/pepe/ququ/"
  }
