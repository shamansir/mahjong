module Flower where

import Prelude


import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)
import Game.Hint (class Hint, hint)
import Game.Doc (class Doc)
import Game.Pictogram (class HasPicto)
import Game.Batch (class Batch)
import Game.Enc (class Enc)


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


instance HasPicto Flower where
  picto = case _ of
    Plum -> "🀢"
    Orchid -> "🀣"
    Chrysanthenium -> "🀥"
    FBamboo -> "🀤"


instance Hint Flower where
  hint = case _ of
    Plum -> "{P}"
    Orchid -> "{O}"
    Chrysanthenium -> "{C}"
    FBamboo -> "{B}"


instance Doc Flower where
  doc = case _ of
    Plum -> "Plum Flower (mei / 梅)"
    Orchid -> "Orchid Flower (lan / 兰 or 蘭)"
    Chrysanthenium -> "Chrysanthenium Flower (ju / 菊)"
    FBamboo -> "Bamboo Flower (zhu / 竹)"


instance Batch Flower where
  all = flowers


instance Enc Flower where
  encode = hint


instance Eq Flower where eq = intEq
instance Ord Flower where compare = intCompare
