module Season where

import Prelude


import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)


data Season
  = Spring -- 春
  | Summer -- 夏
  | Autumn -- 秋
  | Winter -- 冬



seasons :: Set Season
seasons = Set.fromFoldable [ Spring, Summer, Autumn, Winter ]


instance OrdInt Season where
  position = case _ of
    Spring -> 0
    Summer -> 1
    Autumn -> 2
    Winter -> 3


instance Eq Season where eq = intEq
instance Ord Season where compare = intCompare
