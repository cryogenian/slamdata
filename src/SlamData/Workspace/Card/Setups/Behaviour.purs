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

module SlamData.Workspace.Card.Setups.Behaviour where

import SlamData.Prelude

type Behaviour s m =
  { synchronize ∷ s → s
  , load ∷ m → s → s
  , save ∷ s → m
  }

defaultModel ∷ ∀ m s. Behaviour s m → m → s → m
defaultModel b m s = b.save $ b.synchronize $ b.load m s
