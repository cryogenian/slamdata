module Api.Common where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Aff (Aff())
import Network.HTTP.StatusCode (StatusCode(..))
import Network.HTTP.Affjax (Affjax(), AJAX())
import Data.Int (fromNumber, toNumber)

successStatus :: StatusCode
successStatus = StatusCode $ fromNumber 200

succeeded :: StatusCode -> Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = toNumber int

getResponse :: forall a e. String -> Affjax e a -> Aff (ajax :: AJAX | e) a 
getResponse msg affjax = do 
  res <- affjax
  if not $ succeeded res.status
    then throwError $ error msg
    else pure res.response
