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

module Dashboard.Dialog.Component where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either())
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), maybe, fromMaybe, isNothing)
import Dialog.Embed as Embed
import Dialog.Error as Error
import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, prjQuery, prjSlot)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B
import Notebook.Common (Slam())
import Render.Common (fadeWhen)

data Dialog
  = Error String
  | Embed String

type State = Maybe Dialog
initialState :: State
initialState = Nothing

data Query a
  = Dismiss a
  | Show Dialog a

type ChildState = Either Error.State Embed.State
type ChildQuery = Coproduct Error.Query Embed.Query
type ErrorSlot = String
type EmbedSlot = String
type ChildSlot = Either ErrorSlot EmbedSlot

cpError :: ChildPath
           Error.State ChildState
           Error.Query ChildQuery
           ErrorSlot ChildSlot
cpError = cpL

cpEmbed :: ChildPath
           Embed.State ChildState
           Embed.Query ChildQuery
           EmbedSlot ChildSlot
cpEmbed = cpR

type StateP = InstalledState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type Algebra = ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: RenderParent State ChildState Query ChildQuery Slam ChildSlot
render state =
  H.div [ P.classes ([B.modal] <> fadeWhen (isNothing state))
        , E.onClick (E.input_ Dismiss)
        ]
  $ maybe [ ] (pure <<< dialog) state
  where
  dialog (Error str) =
    H.slot' cpError str \_ -> { component: Error.comp
                              , initialState: Error.State str
                              }
  dialog (Embed str) =
    H.slot' cpEmbed str \_ -> { component: Embed.comp
                              , initialState: Embed.State str
                              }

eval :: EvalParent Query State ChildState Query ChildQuery Slam ChildSlot
eval (Dismiss next) = modify (const Nothing) $> next
eval (Show d next) = modify (const $ pure d) $> next


peek :: forall a. ChildF ChildSlot ChildQuery a -> Algebra Unit
peek (ChildF slot query) =
  fromMaybe (pure unit)
  $   (errorPeek <$> prjSlot cpError slot <*> prjQuery cpError query)
  <|> (embedPeek <$> prjSlot cpEmbed slot <*> prjQuery cpEmbed query)

errorPeek :: forall a. ErrorSlot -> Error.Query a -> Algebra Unit
errorPeek _ (Error.Dismiss _) = modify (const Nothing)

embedPeek :: forall a. EmbedSlot -> Embed.Query a -> Algebra Unit
embedPeek _ (Embed.Dismiss _) = modify (const Nothing)
embedPeek _ _ = pure unit
