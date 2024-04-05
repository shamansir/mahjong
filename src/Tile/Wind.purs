module Wind where

import Prelude

import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)

import Game.Hint (class Hint, hint)
import Game.Doc (class Doc)
import Game.Pictogram (class HasPicto)
import Game.Batch (class Batch)
import Game.Enc (class Enc)


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


instance HasPicto Wind where
  picto = case _ of
    East -> "🀀"
    South -> "🀁"
    West -> "🀂"
    North -> "🀃"


instance Hint Wind where
  hint = case _ of
    East -> "[E]"
    South -> "[S]"
    West -> "[W]"
    North -> "[N]"


instance Doc Wind where
  doc = case _ of
    East -> "East Wind; Ton / 東"
    South -> "South Wind; Nan / 南"
    West -> "West Wind; Shau / Shā / 西"
    North -> "North Wind; Peu / Pē / 北"


instance Batch Wind where
  all = winds


instance Enc Wind where
  encode = hint


instance Eq Wind where eq = intEq
instance Ord Wind where compare = intCompare


data Prevalent
    = PrevalentEast  -- 東
    | PrevalentSouth -- 南


prevalents :: Set Prevalent
prevalents = Set.fromFoldable [ PrevalentEast, PrevalentSouth ]


instance HasPicto Prevalent where
  picto = case _ of
    PrevalentEast -> "東"
    PrevalentSouth -> "南"


instance Hint Prevalent where
  hint = case _ of
    PrevalentEast -> "|E|"
    PrevalentSouth -> "|S|"


instance Doc Prevalent where
  doc = case _ of
    PrevalentEast -> "Prevalent East (東)"
    PrevalentSouth -> "Prevalent South (南)"


instance Batch Prevalent where
  all = prevalents


instance Enc Prevalent where
  encode = hint


derive instance Eq Prevalent
derive instance Ord Prevalent