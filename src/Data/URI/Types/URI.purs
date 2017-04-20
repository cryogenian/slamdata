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

module Data.URI.Types.URI where

import Data.URI.Types
import Data.Maybe (Maybe)

uriScheme ∷ URI → Maybe URIScheme
uriScheme (URI x _ _ _) =
  x

hierarchicalPart ∷ URI → HierarchicalPart
hierarchicalPart (URI _ x _ _) =
  x

query ∷ URI → Maybe Query
query (URI _ _ x _) =
  x

fragment ∷ URI → Maybe Fragment
fragment (URI _ _ _ x) =
  x

