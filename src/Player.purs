module Player where

import Prelude


import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)

import Game.Hint (class Hint, hint)
import Game.Doc (class Doc)
import Game.Pictogram (class HasPicto)
import Game.Batch (class Batch)
import Game.Enc (class Enc)


-- FIXME: data Player = Player Wind
data Player
    = PlayerN
    | PlayerW
    | PlayerS
    | PlayerE


players :: Set Player
players = Set.fromFoldable [ PlayerE, PlayerS, PlayerW, PlayerN ]


instance OrdInt Player where
  position = case _ of
    PlayerE -> 0
    PlayerS -> 1
    PlayerW -> 2
    PlayerN -> 3


{-
instance HasPicto Player where
  picto = case _ of
    PlayerE -> "🀀"
    PlayerS -> "🀁"
    PlayerW -> "🀂"
    PlayerN -> "🀃"
-}


instance Hint Player where
  hint = case _ of
    PlayerE -> "`E`"
    PlayerS -> "`S`"
    PlayerW -> "`W`"
    PlayerN -> "`N`"


instance Doc Player where
  doc = case _ of
    PlayerE -> "Player East / 東"
    PlayerS -> "Player South / 南"
    PlayerW -> "Player West / 西"
    PlayerN -> "Player North / 北"


instance Batch Player where
  all = players


instance Enc Player where
  encode = hint


instance Eq Player where eq = intEq
instance Ord Player where compare = intCompare