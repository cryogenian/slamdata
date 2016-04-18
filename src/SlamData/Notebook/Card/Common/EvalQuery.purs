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

module SlamData.Notebook.Card.Common.EvalQuery
  ( CardEvalQuery(..)
  , CardEvalResult
  , CardEvalResultP
  , CardEvalInputP
  , CardEvalInputPre
  , CardEvalInput
  , CardSetupInfoP
  , CardSetupInfo
  , CardEvalT
  , runCardEvalT
  , temporaryOutputResource
  , prepareCardEvalInput
  , liftWithCanceler
  , liftWithCanceler'
  , liftWithCancelerP
  , liftWithCancelerP'
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Canceler)
import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET
import Control.Monad.Writer.Class as WC
import Control.Monad.Writer.Trans as WT

import Data.Argonaut.Core (Json)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P

import SlamData.Notebook.Card.CardId as CID
import SlamData.Notebook.Card.Port (Port)
import SlamData.Notebook.Card.Port.VarMap as Port
import SlamData.Effects (Slam, SlamDataEffects)

import Utils.Path (DirPath, FilePath)

import Halogen (ParentDSL, ComponentDSL)
import Halogen.Component.Utils as Hu

type CardEvalInputP r =
  { notebookPath ∷ Maybe DirPath
  , inputPort ∷ Maybe Port
  , cardId ∷ CID.CardId
  , globalVarMap ∷ Port.VarMap
  | r
  }

type CardEvalInputPre = CardEvalInputP ()
type CardEvalInput =
  CardEvalInputP
    ( cachingEnabled ∷ Maybe Boolean
    )

type CardSetupInfoP r =
  { notebookPath ∷ Maybe DirPath
  , inputPort ∷ Port
  , cardId ∷ CID.CardId
  | r}

type CardSetupInfo =
  CardSetupInfoP ()

prepareCardEvalInput
  ∷ Maybe Boolean
  → CardEvalInputPre
  → CardEvalInput
prepareCardEvalInput cachingEnabled { notebookPath, inputPort, cardId, globalVarMap } =
  { notebookPath
  , inputPort
  , cardId
  , cachingEnabled
  , globalVarMap
  }

temporaryOutputResource
  ∷ ∀ r
  . {notebookPath ∷ Maybe DirPath, cardId ∷ CID.CardId | r}
  → FilePath
temporaryOutputResource info =
  outputDirectory </> outputFile
  where
    outputDirectory =
      filterMaybe (_ == P.rootDir) info.notebookPath #
        fromMaybe (P.rootDir </> P.dir ".tmp")

    outputFile =
      P.file $ "out" ⊕ CID.cardIdToString info.cardId

    filterMaybe ∷ ∀ a. (a → Boolean) → Maybe a → Maybe a
    filterMaybe p m =
      m >>= \x →
        if p x then Nothing else pure x

-- | The query algebra shared by the inner parts of a card component.
-- |
-- | - `EvalCard` is a command sent from the notebook that runs the card. An
-- |   optional input value (the output from another card) is provided, and a
-- |   continuation for the evaluation result to be returned to.
-- | - `SetupCard` is will be called when the card is being added as a linked
-- |   card from another, passing through the current input port value so the
-- |   current card can set its state based on that. Used to pull a VarMap
-- |   through for autocomplete purposes, or for the search card to be able to
-- |   auto-select the parent card's result set as the resource, etc.
-- | - `NotifyRunCard` allows the card to notify the notebook that it should be
-- |   run - the card cannot run itself directly.
data CardEvalQuery a
  = EvalCard CardEvalInput (CardEvalResult → a)
  | SetupCard CardSetupInfo a
  | NotifyRunCard a
  | NotifyStopCard a
  | SetCanceler (Canceler SlamDataEffects) a
  | Save (Json → a)
  | Load Json a

-- | The result value produced when evaluating a card.
-- |
-- | - `output` is the value that this card component produces that is taken as
-- |   the input for dependant cards. Not every card produces an output.
-- | - `messages` is for any error or status messages that arise during
-- |   evaluation. `Left` values are errors, `Right` values are informational
-- |   messages.
type CardEvalResultP a =
  { output ∷ Maybe a
  , messages ∷ Array (Either String String)
  }

type CardEvalResult = CardEvalResultP Port

type CardEvalTP m = ET.ExceptT String (WT.WriterT (Array String) m)
newtype CardEvalT m a = CardEvalT (CardEvalTP m a)

getCardEvalT ∷ ∀ m a. CardEvalT m a → CardEvalTP m a
getCardEvalT (CardEvalT m) = m

instance functorCardEvalT ∷ (Functor m) ⇒ Functor (CardEvalT m) where
  map f = getCardEvalT ⋙ map f ⋙ CardEvalT

instance applyCardEvalT ∷ (Apply m) ⇒ Apply (CardEvalT m) where
  apply (CardEvalT f) = getCardEvalT ⋙ apply f ⋙ CardEvalT

instance applicativeCardEvalT ∷ (Applicative m) ⇒ Applicative (CardEvalT m) where
  pure = pure ⋙ CardEvalT

instance bindCardEvalT ∷ (Monad m) ⇒ Bind (CardEvalT m) where
  bind (CardEvalT m) = (_ ⋙ getCardEvalT) ⋙ bind m ⋙ CardEvalT

instance monadCardEvalT ∷ (Monad m) ⇒ Monad (CardEvalT m)

instance monadTransCardEvalT ∷ MonadTrans CardEvalT where
  lift = lift ⋙ lift ⋙ CardEvalT

instance monadWriterCardEvalT ∷ (Monad m) ⇒ WC.MonadWriter (Array String) (CardEvalT m) where
  writer = WC.writer ⋙ lift ⋙ CardEvalT
  listen = getCardEvalT ⋙ WC.listen ⋙ CardEvalT
  pass = getCardEvalT ⋙ WC.pass ⋙ CardEvalT

instance monadErrorCardEvalT ∷ (Monad m) ⇒ EC.MonadError String (CardEvalT m) where
  throwError = EC.throwError ⋙ CardEvalT
  catchError (CardEvalT m) = CardEvalT ∘ EC.catchError m ∘ (getCardEvalT ∘ _)

runCardEvalT
  ∷ ∀ m a
  . (Functor m)
  ⇒ CardEvalT m a
  → m (CardEvalResultP a)
runCardEvalT (CardEvalT m) =
  WT.runWriterT (ET.runExceptT m) <#> uncurry \r ms →
    { output: either (const Nothing) Just r
    , messages: either (Left ⋙ pure) (const []) r ⊕ map Right ms
    }


liftWithCancelerP
  ∷ ∀ a state slot innerQuery innerState
  . Slam a
  → ParentDSL
      state innerState
      CardEvalQuery innerQuery
      Slam slot a
liftWithCancelerP =
  Hu.liftWithCanceler' SetCanceler

liftWithCancelerP'
  ∷ ∀ a state innerState innerQuery query slot
  . Slam a
  → ParentDSL
      state innerState
      (Coproduct CardEvalQuery query) innerQuery
      Slam slot a
liftWithCancelerP' =
  Hu.liftWithCanceler' (\c u → left $ SetCanceler c u)

liftWithCanceler
  ∷ ∀ a state
  . Slam a
  → ComponentDSL state CardEvalQuery Slam a
liftWithCanceler =
  Hu.liftWithCanceler SetCanceler

liftWithCanceler'
  ∷ ∀ state query a
  . Slam a
  → ComponentDSL state (Coproduct CardEvalQuery query) Slam a
liftWithCanceler' =
  Hu.liftWithCanceler (\c u → left $ SetCanceler c u)
