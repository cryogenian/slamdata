module Node.Rimraf where

import Prelude

import Control.Monad.Aff (Aff)
import Node.FS (FS)

foreign import rimraf :: forall e. String -> Aff (fs :: FS|e) Unit

