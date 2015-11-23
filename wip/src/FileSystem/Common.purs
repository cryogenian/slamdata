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

module FileSystem.Common where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Free (Free())

import Halogen

import FileSystem.Effects

-- | Target of Halogen DSL compilation
type Slam = Aff FileSystemEffects

forceRerender :: forall s f g. (Applicative g) => Free (HalogenF s f g) Unit
forceRerender = liftH $ pure unit

forceRerender'
  :: forall s s' f f' g p. (Applicative g) => ParentDSL s s' f f' g p Unit
forceRerender' = liftH $ liftH $ pure unit

liftEff''
  :: forall s s' f f' p. Natural (Eff FileSystemEffects) (ParentDSL s s' f f' Slam p)
liftEff'' = liftH <<< liftEff'

liftAff''
  :: forall s s' f f' p. Natural (Aff FileSystemEffects) (ParentDSL s s' f f' Slam p)
liftAff'' = liftH <<< liftAff'
