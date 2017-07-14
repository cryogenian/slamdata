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

module Data.Visibility (Visibility(..), isVisible, isInvisible, toggleVisibility) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Visibility = Visible | Invisible

derive instance eqVisibility ∷ Eq Visibility
derive instance ordVisibility ∷ Ord Visibility
derive instance genericVisibility ∷ Generic Visibility _
instance showVisibility ∷ Show Visibility where show = genericShow

isVisible ∷ Visibility → Boolean
isVisible Visible = true
isVisible _ = false

isInvisible ∷ Visibility → Boolean
isInvisible Invisible = true
isInvisible _ = false

toggleVisibility ∷ Visibility → Visibility
toggleVisibility Visible = Invisible
toggleVisibility Invisible = Visible
