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

module Optic.Refractor.Prism where

import Prelude
import Data.Either (either, Either(..))
import Data.Maybe (maybe, Maybe(..))

import Optic.Prism (prism, prism')
import Optic.Types (Prism(), PrismP())

_Left :: forall a b c. Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left <<< Right)

_Right :: forall a b c. Prism (Either a b) (Either a c) b c
_Right = prism Right $ either (Left <<< Left) Right

_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

_Nothing :: forall a b. PrismP (Maybe a) Unit
_Nothing = prism' (const Nothing) $ maybe Nothing (const $ Just unit)
