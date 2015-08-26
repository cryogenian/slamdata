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

-- Hope we can drop to use this module and switch to `later` from `purescript-aff`
module Control.Timer where

import Prelude
import Control.Monad.Eff (Eff())

foreign import data TIMER :: !
foreign import data Timeout :: *
foreign import data Interval :: *

foreign import timeout :: forall a e. Int -> Eff (timer :: TIMER|e) a -> Eff (timer ::TIMER|e) Timeout 
foreign import clearTimeout :: forall e. Timeout -> Eff (timer :: TIMER|e) Unit
foreign import interval :: forall a e. Int -> Eff (timer :: TIMER|e) a -> Eff (timer :: TIMER|e) Interval
foreign import clearInterval :: forall e. Interval -> Eff (timer :: TIMER|e) Unit
