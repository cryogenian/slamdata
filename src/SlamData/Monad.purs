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

module SlamData.Monad where

import SlamData.Prelude

import Control.Applicative.Free (FreeAp, hoistFreeAp, liftFreeAp, retractFreeAp)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Fork (class MonadFork, fork)
import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.Throw (class MonadThrow)
import Control.Parallel (parallel, sequential)
import Control.UI.Browser (locationObject)

import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P

import DOM.HTML.Location (setHash)

import OIDC.Crypt.Types as OIDC

import Quasar.Advanced.QuasarAF as QA

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Monad.ForkF as FF
import SlamData.Notification as N
import SlamData.Quasar.Aff (runQuasarF)
import SlamData.Quasar.Auth (class QuasarAuthDSL)
import SlamData.Quasar.Auth.Authentication as Auth
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Workspace.Class (class WorkspaceDSL)
import SlamData.Workspace.Deck.DeckId as DeckId
import SlamData.Workspace.Routing as Routing
import SlamData.Monad.Auth (getIdTokenSilently)

import Utils (hush)

type Slam = SlamM SlamDataEffects

data SlamF eff a
  = Aff (Aff eff a)
  | GetAuthIdToken (Maybe OIDC.IdToken → a)
  | Quasar (QA.QuasarAFC a)
  | Notify N.NotificationOptions a
  | Halt GE.GlobalError a
  | Par (SlamA eff a)
  | Fork (FF.Fork (SlamM eff) a)
  | Ask (Wiring → a)
  | Navigate Routing.Routes a

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

instance monadEffSlamM ∷ MonadEff eff (SlamM eff) where
  liftEff = SlamM ∘ liftF ∘ Aff ∘ liftEff

instance monadAffSlamM ∷ MonadAff eff (SlamM eff) where
  liftAff = SlamM ∘ liftF ∘ Aff

instance affableSlamM ∷ Affable eff (SlamM eff) where
  fromAff = SlamM ∘ liftF ∘ Aff

instance monadParSlamM ∷ Parallel (SlamA eff) (SlamM eff) where
  parallel = SlamA ∘ liftFreeAp
  sequential = SlamM ∘ liftF ∘ Par

instance monadForkSlamM ∷ MonadFork Error (SlamM eff) where
  fork a = map liftAff <$> SlamM (liftF $ Fork $ FF.fork a)

instance monadAskSlamM ∷ MonadAsk Wiring (SlamM eff) where
  ask = SlamM $ liftF $ Ask id

instance monadRecSlamM ∷ MonadRec (SlamM eff) where
  tailRecM k a = k a >>= case _ of
    Loop b → tailRecM k b
    Done r → pure r

instance monadThrow ∷ MonadThrow Error (SlamM eff) where
  throw = liftAff ∘ throwError

instance quasarAuthDSLSlamM ∷ QuasarAuthDSL (SlamM eff) where
  getIdToken = SlamM $ liftF $ GetAuthIdToken id

instance quasarDSLSlamM ∷ QuasarDSL (SlamM eff) where
  liftQuasar = SlamM ∘ liftF ∘ Quasar

instance notifyDSLSlamM ∷ N.NotifyDSL (SlamM eff) where
  notify notification detail timeout actionOptions =
    SlamM $ liftF $ Notify { notification, detail, timeout, actionOptions } unit

instance globalErrorDSLSlamM ∷ GE.GlobalErrorDSL (SlamM eff) where
  raiseGlobalError = SlamM ∘ liftF ∘ flip Halt unit

instance workspaceDSLSlamM ∷ WorkspaceDSL (SlamM eff) where
  navigate = SlamM ∘ liftF ∘ flip Navigate unit

newtype SlamA eff a = SlamA (FreeAp (SlamM eff) a)

derive newtype instance functorSlamA :: Functor (SlamA eff)
derive newtype instance applySlamA :: Apply (SlamA eff)
derive newtype instance applicativeSlamA :: Applicative (SlamA eff)

--------------------------------------------------------------------------------

runSlam ∷ Wiring → Slam ~> Aff SlamDataEffects
runSlam wiring@(Wiring.Wiring { auth, bus }) = foldFree go ∘ unSlamM
  where

  go ∷ SlamF SlamDataEffects ~> Aff SlamDataEffects
  go = case _ of
    Aff aff →
      aff
    GetAuthIdToken k → do
      idToken ← hush <$> getIdTokenSilently auth.allowedModes auth.requestToken
      case idToken of
        Just (Left error) →
          for_ (Auth.toNotificationOptions error) \opts →
            Bus.write opts bus.notify
        _ →
          pure unit
      pure $ k $ maybe Nothing hush idToken
    Quasar qf → do
      idToken ← hush <$> getIdTokenSilently auth.allowedModes auth.requestToken
      case idToken of
        Just (Left error) →
          for_ (Auth.toNotificationOptions error) \opts →
            Bus.write opts bus.notify
        _ →
          pure unit
      runQuasarF (maybe Nothing hush idToken) qf
    Notify no a → do
      Bus.write no bus.notify
      pure a
    Halt err a → do
      Bus.write (GE.toNotificationOptions err) bus.notify
      pure a
    Par (SlamA p) →
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< runSlam wiring) p
    Fork f →
      goFork f
    Ask k →
      pure (k wiring)
    Navigate (Routing.WorkspaceRoute path deckIds action varMaps) a → do
      let
        path' = foldr (\y x → x </> P.dir (DeckId.toString y)) path deckIds
        hash  = Routing.mkWorkspaceHash path' action varMaps
      liftEff $ locationObject >>= setHash hash
      pure a

  goFork ∷ FF.Fork Slam ~> Aff SlamDataEffects
  goFork = FF.unFork \(FF.ForkF fx k) →
    k ∘ map unsafeCoerceAff <$> fork (runSlam wiring fx)
