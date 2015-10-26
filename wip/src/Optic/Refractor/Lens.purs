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

module Optic.Refractor.Lens where

import Prelude
import Data.Tuple (Tuple(..))
import Optic.Types (Lens())

_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
_1 a2fb (Tuple a c) = (\b -> Tuple b c) <$> a2fb a

_2 :: forall a b c. Lens (Tuple a b) (Tuple a c) b c
_2 a2fb (Tuple a b) = Tuple a <$> a2fb b
