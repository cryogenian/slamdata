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

module SlamData.Workspace.Card.Eval.Monad
  ( CardEval
  , CardLog
  , CardState
  , CardError
  , CardEnv(..)
  , CardResult
  , CardEvalM
  , addSource
  , addCache
  , addSources
  , addCaches
  , additionalSources
  , temporaryOutputResource
  , localUrlVarMap
  , throw
  , liftQ
  , runCardEvalM
  , module SlamData.Workspace.Card.Eval.State
  , module SlamData.Workspace.Deck.AdditionalSource
  ) where

import SlamData.Prelude hiding (throwError)

import Control.Applicative.Free (FreeAp, liftFreeAp)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, liftF, resume)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Throw as Throw
import Control.Monad.Writer.Class (class MonadTell, tell)

import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Path
import Data.Set (Set)
import Data.Set as Set

import Quasar.Advanced.QuasarAF as QA
import Quasar.Error (QError)

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SlamData.Quasar.Error (msgToQError)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Eval.State (EvalState(..))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource(..))
import SlamData.Workspace.Deck.DeckId as DID
import Utils.Path (DirPath, FilePath)

type CardEval = CardEvalM SlamDataEffects

type CardLog = Set AdditionalSource

type CardState = Maybe EvalState

type CardError = QError

type CardResult a =
  { output ∷ Either QError a
  , sources ∷ Set.Set AdditionalSource
  , state ∷ CardState
  }

newtype CardEnv = CardEnv
  { path ∷ DirPath
  , coord ∷ DID.DeckId × CID.CardId
  , urlVarMaps ∷ Map.Map DID.DeckId Port.URLVarMap
  }

data CardEvalF eff a
  = Aff (Aff eff a)
  | Par (CardEvalA eff a)
  | Quasar (QA.QuasarAFC a)
  | Tell (a × Set AdditionalSource)
  | State (CardState → a × CardState)
  | Ask (CardEnv → a)
  | Throw QError

instance functorCardEvalF ∷ Functor (CardEvalF eff) where
  map f = case _ of
    Aff aff   → Aff (f <$> aff)
    Par par   → Par (f <$> par)
    Quasar q  → Quasar (f <$> q)
    Tell a    → Tell (lmap f a)
    State a   → State (lmap f <$> a)
    Ask a     → Ask (f <$> a)
    Throw err → Throw err

newtype CardEvalA eff a = CardEvalA (FreeAp (CardEvalM eff) a)

derive newtype instance functorCardEvalA :: Functor (CardEvalA eff)
derive newtype instance applyCardEvalA :: Apply (CardEvalA eff)
derive newtype instance applicativeCardEvalA :: Applicative (CardEvalA eff)

newtype CardEvalM eff a = CardEvalM (Free (CardEvalF eff) a)

unCardEvalM ∷ ∀ eff. CardEvalM eff ~> Free (CardEvalF eff)
unCardEvalM (CardEvalM a) = a

instance functorCardEvalM ∷ Functor (CardEvalM eff) where
  map f (CardEvalM a) = CardEvalM (map f a)

instance applyCardEvalM ∷ Apply (CardEvalM eff) where
  apply (CardEvalM a) (CardEvalM b) = CardEvalM (a <*> b)

instance applicativeCardEvalM ∷ Applicative (CardEvalM eff) where
  pure = CardEvalM ∘ pure

instance bindCardEvalM ∷ Bind (CardEvalM eff) where
  bind (CardEvalM a) f = CardEvalM (a >>= unCardEvalM ∘ f)

instance monadCardEvalM ∷ Monad (CardEvalM eff)

instance monadThrowCardEvalM ∷ MonadThrow QError (CardEvalM eff) where
  throw = CardEvalM ∘ liftF ∘ Throw

instance monadStateCardEvalM ∷ MonadState (Maybe EvalState) (CardEvalM eff) where
  state = CardEvalM ∘ liftF ∘ State

instance monadAskCardEvalM ∷ MonadAsk CardEnv (CardEvalM eff) where
  ask = CardEvalM (liftF (Ask id))

instance monadTellCardEvalM ∷ MonadTell (Set AdditionalSource) (CardEvalM eff) where
  tell as = CardEvalM $ liftF $ Tell (unit × as)

instance monadEffCardEvalM ∷ MonadEff eff (CardEvalM eff) where
  liftEff = CardEvalM ∘ liftF ∘ Aff ∘ liftEff

instance monadAffCardEvalM ∷ MonadAff eff (CardEvalM eff) where
  liftAff = CardEvalM ∘ liftF ∘ Aff

instance affableCardEvalM ∷ Affable eff (CardEvalM eff) where
  fromAff = CardEvalM ∘ liftF ∘ Aff

instance parallelCardEvalM ∷ Parallel (CardEvalA eff) (CardEvalM eff) where
  parallel = CardEvalA ∘ liftFreeAp
  sequential = CardEvalM ∘ liftF ∘ Par

instance quasarDSLCardEvalM ∷ QuasarDSL (CardEvalM eff) where
  liftQuasar = CardEvalM ∘ liftF ∘ Quasar

addSource ∷ ∀ m. (MonadTell (Set AdditionalSource) m) ⇒ FilePath → m Unit
addSource fp = tell (Set.singleton (Source fp))

addCache ∷ ∀ m. (MonadTell (Set AdditionalSource) m) ⇒ FilePath → m Unit
addCache fp = tell (Set.singleton (Cache fp))

addSources ∷ ∀ f m. (Foldable f, MonadTell (Set AdditionalSource) m) ⇒ f FilePath → m Unit
addSources fps = tell (foldMap (Set.singleton ∘ Source) fps)

addCaches ∷ ∀ f m. (Foldable f, MonadTell (Set AdditionalSource) m) ⇒ f FilePath → m Unit
addCaches fps = tell (foldMap (Set.singleton ∘ Cache) fps)

additionalSources ∷ ∀ f m. (Foldable f, MonadTell (Set AdditionalSource) m) ⇒ f AdditionalSource → m Unit
additionalSources = tell ∘ foldMap Set.singleton

temporaryOutputResource ∷ ∀ m. (MonadReader CardEnv m) ⇒ m FilePath
temporaryOutputResource = do
  CardEnv { path, coord: deckId × cardId } ← ask
  pure $ path
    </> Path.dir ".tmp"
    </> Path.dir (DID.toString deckId)
    </> Path.file ("out" ⊕ CID.toString cardId)

localUrlVarMap ∷ ∀ m. (MonadReader CardEnv m) ⇒ m Port.URLVarMap
localUrlVarMap = do
  CardEnv { coord, urlVarMaps } ← ask
  pure
    (fromMaybe mempty
      (Map.lookup (fst coord) urlVarMaps))

throw ∷ ∀ m a. (MonadThrow QError m) ⇒ String → m a
throw = Throw.throw ∘ msgToQError

liftQ ∷ ∀ m a. (MonadThrow QError m) ⇒ m (Either QError a) → m a
liftQ = flip bind (either Throw.throw pure)

runCardEvalM
  ∷ ∀ eff m a
  . ( QuasarDSL m
    , Affable eff m
    , Monad m
    )
  ⇒ CardEnv
  → CardState
  → CardEvalM eff a
  → m (CardResult a)
runCardEvalM env initialState (CardEvalM ce) = go initialState Set.empty ce
  where
    go st as ce' = case resume ce' of
      Left ctr →
        case ctr of
          Aff aff →
            fromAff aff >>= go st as
          -- TODO: The merging of state isn't possible without a Monoid
          -- constraint here. Really we don't want Par in general, we just
          -- need Par for Quasar requests. For now, don't Par anything that
          -- works with card state.
          Par p →
            -- TODO^2: ummm?
            -- sequential $ retractFreeAp $ hoistFreeAp (parallel <<< runCardEvalM env st) p
            fromAff empty
            -- flip unPar par \(ParF f fx fy) → do
            --   { x, y } ←
            --     sequential $
            --       { x: _, y: _ }
            --         <$> parallel (runCardEvalM env st fx)
            --         <*> parallel (runCardEvalM env st fy)
            --   case x.output, y.output of
            --     Left err, _ →
            --       pure { output: Left err, sources: as, state: st }
            --     _, Left err →
            --       pure { output: Left err, sources: as, state: st }
            --     Right x', Right y' →
            --       go y.state (as <> x.sources <> y.sources) (f x' y')
          Quasar q →
            liftQuasar q >>= go st as
          Tell (n × as') →
            go st (as <> as') n
          State k →
            let
              res = k st
            in
              go (snd res) as (fst res)
          Ask k →
            go st as (k env)
          Throw err →
            pure { output: Left err, sources: as, state: st }
      Right a →
        pure { output: Right a, sources: as, state: st }
