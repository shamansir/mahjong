module Valued where

import Prelude


class Valued a where
  valueOf :: a -> Int
