module Flower where

import Prelude


import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)


data Flower
  = Plum -- 梅
  | Orchid -- 兰 or 蘭
  | Chrysanthenium -- 菊
  | FBamboo -- 竹



flowers :: Set Flower
flowers = Set.fromFoldable [ Plum, Orchid, Chrysanthenium, FBamboo ]


instance OrdInt Flower where
  position = case _ of
    Plum -> 0
    Orchid -> 1
    Chrysanthenium -> 2
    FBamboo -> 3


instance Show Flower where
  show = case _ of
    Plum -> "🀢"
    Orchid -> "🀣"
    Chrysanthenium -> "🀥"
    FBamboo -> "🀤"


instance Eq Flower where eq = intEq
instance Ord Flower where compare = intCompare
