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

module Utils.Log where

import Prelude

spyF :: forall a m. (Applicative m) => a -> m Unit
spyF a = let x = spy a in pure unit

spyM :: forall a m. (Applicative m) => a -> m a
spyM a = pure $ spy a

foreign import spy :: forall a. a -> a
