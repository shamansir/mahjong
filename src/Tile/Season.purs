module Season where

import Prelude


import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)

import Game.Hint (class Hint, hint)
import Game.Doc (class Doc)
import Game.Pictogram (class HasPicto)
import Game.Batch (class Batch)
import Game.Enc (class Enc)


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


instance HasPicto Season where
  picto = case _ of
    Spring -> "🀦"
    Summer -> "🀧"
    Autumn -> "🀨"
    Winter -> "🀩"


instance Hint Season where
  hint = case _ of
    Spring -> "!SPR!"
    Summer -> "!SUM!"
    Autumn -> "!AUT!"
    Winter -> "!WIN!"


instance Doc Season where
  doc = case _ of
    Spring -> "Spring Season (春)"
    Summer -> "Summer Season (夏)"
    Autumn -> "Autumn Season (秋)"
    Winter -> "Winter Season (冬)"


instance Batch Season where
  all = seasons


instance Enc Season where
  encode = hint


instance Eq Season where eq = intEq
instance Ord Season where compare = intCompare
