module Dashboard.Effects where

import Ace.Types (ACE())
import Halogen (HalogenEffects())
import Network.HTTP.Affjax (AJAX())

type DashboardEffects = HalogenEffects DashboardRawEffects

type DashboardRawEffects =
  ( ajax :: AJAX
  , ace :: ACE
  )
