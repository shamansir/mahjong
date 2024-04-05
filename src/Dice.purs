module Dice where

import Prelude

import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)
import Valued (class Valued, valueOf)
import Game.Hint (class Hint, hint)
import Game.Doc (class Doc)
import Game.Pictogram (class HasPicto)
import Game.Batch (class Batch)
import Game.Enc (class Enc)


data Dice
  = Dice1
  | Dice2
  | Dice3
  | Dice4
  | Dice5
  | Dice6


possibleSides :: Set Dice
possibleSides = Set.fromFoldable [ Dice1, Dice2, Dice3, Dice4, Dice5, Dice6 ]


instance Valued Dice where
  valueOf = case _ of
    Dice1 -> 1
    Dice2 -> 2
    Dice3 -> 3
    Dice4 -> 4
    Dice5 -> 5
    Dice6 -> 6


instance HasPicto Dice where
  picto = case _ of
    Dice1 -> "⚀"
    Dice2 -> "⚁"
    Dice3 -> "⚂"
    Dice4 -> "⚃"
    Dice5 -> "⚄"
    Dice6 -> "⚅"


instance Hint Dice where
  hint = case _ of
    Dice1 -> "(1)"
    Dice2 -> "(2)"
    Dice3 -> "(3)"
    Dice4 -> "(4)"
    Dice5 -> "(5)"
    Dice6 -> "(6)"


instance Doc Dice where
  doc d = "Dice of value " <> hint d


instance Batch Dice where
  all = possibleSides


instance Enc Dice where
  encode = hint


instance OrdInt Dice where position = valueOf
instance Eq Dice where eq = intEq
instance Ord Dice where compare = intCompare
