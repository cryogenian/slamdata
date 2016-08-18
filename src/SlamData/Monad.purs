{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OSlamF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Monad where

import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Fork (class MonadFork, Canceler, cancelWith, fork, hoistCanceler)
import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.Reader (ReaderT, runReaderT, local)
import Control.Parallel.Class (par)

import Quasar.Advanced.QuasarAF as QSlamF

import SlamData.Analytics as A
import SlamData.Notification as N
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Aff (runQuasarF)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.GlobalError as GE
import SlamData.Wiring (Wiring(..))

import Unsafe.Coerce (unsafeCoerce)

type Slam = SlamM SlamDataEffects

--------------------------------------------------------------------------------

data ParF f x y a = ParF (x -> y -> a) (f x) (f y)
data Par (f :: * -> *) a

mkPar :: forall f x y a. ParF f x y a → Par f a
mkPar = unsafeCoerce

unPar :: forall f x y a r. (ParF f x y a → r) → Par f a → r
unPar = unsafeCoerce

--------------------------------------------------------------------------------

data ForkF f x a = ForkF (f x) (Canceler f -> a)
data Fork (f :: * -> *) a

mkFork :: forall f x a. ForkF f x a → Fork f a
mkFork = unsafeCoerce

unFork :: forall f x a r. (ForkF f x a → r) → Fork f a → r
unFork = unsafeCoerce

--------------------------------------------------------------------------------

data SlamF eff a
  = Aff (Aff eff a)
  | Quasar (QSlamF.QuasarAFC a)
  | Track A.Event a
  | Notify N.NotificationOptions a
  | Halt GE.GlobalError a
  | Par (Par (SlamM eff) a)
  | Fork (Fork (SlamM eff) a)
  | Cancel (SlamM eff a) (Canceler (SlamM eff))
  | Ask (Wiring → a)
  | Local (Wiring → Wiring) (SlamM eff a)

newtype SlamM eff a = SlamM (Free (SlamF eff) a)

unSlamM ∷ ∀ eff. SlamM eff ~> Free (SlamF eff)
unSlamM (SlamM a) = a

instance functorSlamM ∷ Functor (SlamM eff) where
  map f (SlamM a) = SlamM (map f a)

instance applySlamM ∷ Apply (SlamM eff) where
  apply (SlamM a) (SlamM b) = SlamM (a <*> b)

instance applicativeSlamM ∷ Applicative (SlamM eff) where
  pure = SlamM ∘ pure

instance bindSlamM ∷ Bind (SlamM eff) where
  bind (SlamM a) f = SlamM (a >>= unSlamM ∘ f)

instance monadSlamM ∷ Monad (SlamM eff)

instance affableSlamM ∷ Affable eff (SlamM eff) where
  fromAff = SlamM ∘ liftF ∘ Aff

instance monadParSlamM ∷ MonadPar (SlamM eff) where
  par f a b = SlamM $ liftF $ Par $ mkPar $ ParF f a b

instance monadForkSlamM ∷ MonadFork (SlamM eff) where
  fork a = SlamM $ liftF $ Fork $ mkFork $ ForkF a id
  cancelWith a c = SlamM $ liftF $ Cancel a c

instance monadReaderSlamM ∷ MonadReader Wiring (SlamM eff) where
  ask = SlamM $ liftF $ Ask id
  local f a = SlamM $ liftF $ Local f a

instance quasarDSLSlamM ∷ QuasarDSL (SlamM eff) where
  liftQuasar = SlamM ∘ liftF ∘ Quasar

instance analyticsDSLSlamM ∷ A.AnalyticsDSL (SlamM eff) where
  track = SlamM ∘ liftF ∘ flip Track unit

instance notifyDSLSlamM ∷ N.NotifyDSL (SlamM eff) where
  notify notification detail timeout =
    SlamM $ liftF $ Notify { notification, detail, timeout } unit

instance globalErrorDSLSlamM ∷ GE.GlobalErrorDSL (SlamM eff) where
  raiseGlobalError = SlamM ∘ liftF ∘ flip Halt unit

--------------------------------------------------------------------------------

runSlam :: Wiring -> Slam ~> Aff SlamDataEffects
runSlam wiring s = runReaderT (unSlam s) wiring

unSlam ∷ Slam ~> ReaderT Wiring (Aff SlamDataEffects)
unSlam = foldFree go ∘ unSlamM
  where

  go ∷ SlamF SlamDataEffects ~> ReaderT Wiring (Aff SlamDataEffects)
  go = case _ of
    Aff aff →
      lift aff
    Quasar qf → do
      Wiring wiring ← ask
      lift $ runQuasarF wiring qf
    Track e a → do
      liftEff $ A.trackEvent e
      pure a
    Notify no a → do
      Wiring wiring ← ask
      lift $ Bus.write no wiring.notify
      pure a
    Halt err a → do
      Wiring wiring ← ask
      lift $ Bus.write err wiring.globalError
      pure a
    Par p →
      goPar p
    Fork f → do
      r ← ask
      goFork r f
    Cancel m c →
      cancelWith (unSlam m) (hoistCanceler unSlam c)
    Ask k →
      k <$> ask
    Local f a →
      local f (unSlam a)

  goPar ∷ Par Slam ~> ReaderT Wiring (Aff SlamDataEffects)
  goPar = unPar \(ParF f x y) →
    par f (unSlam x) (unSlam y)

  goFork ∷ Wiring → Fork Slam ~> ReaderT Wiring (Aff SlamDataEffects)
  goFork r = unFork \(ForkF x k) →
    k ∘ hoistCanceler (SlamM ∘ liftF ∘ Aff ∘ flip runReaderT r)
      <$> fork (unSlam x)
