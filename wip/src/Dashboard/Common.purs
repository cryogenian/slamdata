module Dashboard.Common where

import Prelude

import Dashboard.Effects (DashboardEffects())
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Free (Free())
import Halogen.Component (QueryF())
import Halogen.Query (liftH, HalogenF(), liftEff', liftAff')

type Slam = Aff DashboardEffects

liftEff''
  :: forall a s s' f f' p
   . Eff DashboardEffects a -> Free (HalogenF s f (QueryF s s' f f' Slam p)) a
liftEff'' = liftH <<< liftEff'

liftAff''
  :: forall a s s' f f' p
   . Slam a -> Free (HalogenF s f (QueryF s s' f f' Slam p)) a
liftAff'' = liftH <<< liftAff'
