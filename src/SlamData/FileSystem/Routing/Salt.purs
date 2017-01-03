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

module SlamData.FileSystem.Routing.Salt where

import SlamData.Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)

newtype Salt = Salt String

runSalt :: Salt -> String
runSalt (Salt s) = s

derive instance eqSalt :: Eq Salt

instance showSalt :: Show Salt where
  show (Salt s) = "Salt " <> show s

newSalt :: forall e. Eff (random :: RANDOM | e) Salt
newSalt = Salt <<< show <$> randomInt 1000000 2000000
