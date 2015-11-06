module Notebook.AccessType where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

data AccessType = Editable | ReadOnly

derive instance genericAccessType :: Generic AccessType
instance eqAccessType :: Eq AccessType where eq = gEq
instance ordAccessType :: Ord AccessType where compare = gCompare

instance showAccessType :: Show AccessType where
  show Editable = "Editable"
  show ReadOnly = "ReadOnly"
