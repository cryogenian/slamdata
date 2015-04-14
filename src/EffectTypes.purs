module EffectTypes where

import Control.Monad.Eff.Random
import qualified Control.Timer as Tm
import qualified Network.HTTP.Affjax as Af
import qualified Control.Monad.Aff.AVar as A
import qualified Halogen as Hl
import qualified Utils.File as Uf

type FileComponentEff e = (timer :: Tm.Timer,
                           ajax :: Af.Ajax,
                           avar :: A.AVAR,
                           file :: Uf.ReadFile,
                           random :: Random|e)
type FileAppEff e = Hl.HalogenEffects (FileComponentEff e)
