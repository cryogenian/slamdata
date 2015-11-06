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

module Notebook.Cell.Port (Port(..), _SlamDown) where

import Data.Lens (PrismP(), prism')
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap())

import Text.Markdown.SlamDown (SlamDown())
import Text.Markdown.SlamDown.Html (FormFieldValue())

data Port
  = SlamDown SlamDown
  | VarMap (StrMap FormFieldValue)

_SlamDown :: PrismP Port SlamDown
_SlamDown = prism' SlamDown \v -> case v of
  SlamDown sd -> Just sd
  _ -> Nothing
