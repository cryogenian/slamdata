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

import Config as Config
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Free (Free())
import Control.UI.Browser (encodeURIComponent)
import Data.Maybe (fromMaybe, Maybe())
import Data.Path.Pathy (printPath)
import FileSystem.Effects
import Halogen.Component (QueryF())
import Halogen.Query (liftH, HalogenF(), liftEff', liftAff')
import Model.Salt (Salt(), runSalt)
import Model.Sort (Sort(), sort2string)
import Utils.Path (DirPath())

-- | Target of Halogen DSL compilation
type Slam = Aff FileSystemEffects

forceRerender :: forall s f g. (Applicative g) => Free (HalogenF s f g) Unit
forceRerender = liftH $ pure unit

forceRerender' ::
  forall s s' f f' g p. (Applicative g) =>
  Free (HalogenF s f (QueryF s s' f f' g p)) Unit
forceRerender' = liftH $ liftH $ pure unit

liftEff'' ::
  forall a s s' f f' p.
  Eff FileSystemEffects a -> Free (HalogenF s f (QueryF s s' f f' Slam p)) a
liftEff'' = liftH <<< liftEff'

liftAff'' ::
  forall a s s' f f' p.
  Slam a -> Free (HalogenF s f (QueryF s s' f f' Slam p)) a
liftAff'' = liftH <<< liftAff'
