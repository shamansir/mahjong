module Wind where

import Prelude

import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)


data Wind
  = West -- Shau / Shā / West 西
  | South -- Nan / South 南
  | North -- Peu / Pē / North 北
  | East  -- Ton / East / 東



winds :: Set Wind
winds = Set.fromFoldable [ East, South, West, North ]


instance OrdInt Wind where
  position = case _ of
    East -> 0
    South -> 1
    West -> 2
    North -> 3


instance Eq Wind where eq = intEq
instance Ord Wind where compare = intCompare


data Prevalent
    = PrevalentEast  -- 東
    | PrevalentSouth -- 南