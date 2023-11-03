module Order where

import Prelude


class OrdInt a where
  position :: a -> Int


intEq :: forall a. OrdInt a => a -> a -> Boolean
intEq a b = eq (position a) (position b)


intCompare :: forall a. OrdInt a => a -> a -> Ordering
intCompare a b = compare (position a) (position b)
