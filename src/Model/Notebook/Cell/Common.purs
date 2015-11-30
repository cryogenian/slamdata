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

module Model.Notebook.Cell.Common where

import Optic.Core

_input :: forall a b r. Lens {input :: a | r} {input :: b | r} a b
_input = lens _.input _{input = _}

_table :: forall a b r. Lens {table :: a | r} {table :: b | r} a b
_table = lens _.table _{table = _}

_shouldCacheResults :: forall a b r. Lens {shouldCacheResults :: a | r} {shouldCacheResults :: b | r} a b
_shouldCacheResults = lens _.shouldCacheResults _{shouldCacheResults = _}
