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

module SlamData.Workspace.Deck.DeckPath where

import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import SlamData.Workspace.Deck.DeckId (DeckId, toString)
import Utils.Path (DirPath)

deckPath' ∷ DirPath → DeckId → DirPath
deckPath' path deckId = path </> P.dir (toString deckId)

deckPath ∷ ∀ r. { id ∷ DeckId, path ∷ DirPath | r } → DirPath
deckPath state = deckPath' state.path state.id
