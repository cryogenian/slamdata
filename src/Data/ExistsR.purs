module Data.ExistsR where

import Unsafe.Coerce (unsafeCoerce)

foreign import data ExistsAR ∷ (* → # * → *) → *

mkExistsAR ∷ ∀ a r f. f a r → ExistsAR f
mkExistsAR = unsafeCoerce

runExistsAR ∷ ∀ f t. (∀ a r. f a r → t) → ExistsAR f → t
runExistsAR = unsafeCoerce
