module Utils.SessionStorage
  ( setSessionStorage
  , getSessionStorage
  ) where

import Prelude
import Control.Bind ((>=>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff, MonadEff)
import Data.Argonaut
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Function
import DOM (DOM())

foreign import
  setSessionStorageImpl
    :: forall e
     . Fn2
         String
         String
         (Eff (dom :: DOM | e) Unit)

foreign import
  getSessionStorageImpl
    :: forall e a
     . Fn3
         (Maybe a)
         (a -> Maybe a)
         String
         (Eff (dom :: DOM | e) (Maybe String))

setSessionStorage
  :: forall a e g
   . (EncodeJson a, MonadEff (dom :: DOM | e) g)
  => String
  -> a
  -> g Unit
setSessionStorage key =
  liftEff <<< runFn2 setSessionStorageImpl key <<< printJson <<< encodeJson

getSessionStorage
  :: forall a e g
   . (DecodeJson a, MonadEff (dom :: DOM | e) g)
  => String
  -> g (Either String a)
getSessionStorage key =
  liftEff $
    runFn3 getSessionStorageImpl Nothing Just key <#>
      maybe
        (Left $ "There is no value for key " <> key)
        (jsonParser >=> decodeJson)
