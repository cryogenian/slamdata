module Data.Visibility (Visibility(..), isVisible, isInvisible, toggleVisibility) where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

data Visibility = Visible | Invisible

derive instance genericVisibility :: Generic Visibility
instance eqVisibility :: Eq Visibility where eq = gEq
instance ordVisibility :: Ord Visibility where compare = gCompare

instance showVisibility :: Show Visibility where
  show Visible = "Visible"
  show Invisible = "Invisible"

isVisible :: Visibility -> Boolean
isVisible Visible = true
isVisible _ = false

isInvisible :: Visibility -> Boolean
isInvisible Invisible = true
isInvisible _ = false

toggleVisibility :: Visibility -> Visibility
toggleVisibility Visible = Invisible
toggleVisibility Invisible = Visible
