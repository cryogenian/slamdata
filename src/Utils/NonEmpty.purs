module Utils.NonEmpty where

import Prelude

import Control.Alt (Alt)
import Control.Alternative (Alternative)
import Data.NonEmpty (NonEmpty(), (:|), head, tail, oneOf)
import Data.Maybe as M
import Data.Maybe.Unsafe as Mu
import Data.Array as A


liftNonEmpty
  :: forall f a b
   . (Uncons f)
  => (f a -> f b)
  -> NonEmpty f a
  -> NonEmpty f b
liftNonEmpty fab nefa =
  let
    fa :: f a
    fa = oneOf nefa

    fb :: f b
    fb = fab fa

    mbUnconsed :: M.Maybe { head :: b, tail :: f b}
    mbUnconsed = uncons fb

    unconsed :: {head :: b, tail :: f b }
    unconsed = Mu.fromJust mbUnconsed
  in
   unconsed.head :| unconsed.tail

nonEmptyCons
  :: forall f a
   . (Uncons f)
  => f a
  -> M.Maybe (NonEmpty f a)
nonEmptyCons fa =
  (\x -> x.head :| x.tail) <$> uncons fa

-- mbHead empty = Nothing
-- mbHead (foo <|> bar) = mbHead foo <|> mbHead bar
class (Alternative f) <= Uncons f where
  uncons :: forall a. f a -> M.Maybe { head :: a, tail :: f a }

instance arrayWithHead :: Uncons Array where
  uncons = A.uncons
