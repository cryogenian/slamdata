module Controller.Notebook where

import Control.Monad.Aff
import Data.List
import qualified Halogen as H
import qualified Model.Notebook as M

handler :: forall e. M.Request -> Aff (H.HalogenEffects e) M.Input
handler r = pure $ M.ViewNotebook "" Nil
