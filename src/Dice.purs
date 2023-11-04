module Dice where

import Prelude

import Order (class OrdInt, intEq, intCompare)
import Valued (class Valued, valueOf)


data Dice
  = Dice1
  | Dice2
  | Dice3
  | Dice4
  | Dice5
  | Dice6


instance Valued Dice where
  valueOf = case _ of
    Dice1 -> 1
    Dice2 -> 2
    Dice3 -> 3
    Dice4 -> 4
    Dice5 -> 5
    Dice6 -> 6


instance Show Dice where
  show = case _ of
    Dice1 -> "⚀"
    Dice2 -> "⚁"
    Dice3 -> "⚂"
    Dice4 -> "⚃"
    Dice5 -> "⚄"
    Dice6 -> "⚅"


instance OrdInt Dice where position = valueOf
instance Eq Dice where eq = intEq
instance Ord Dice where compare = intCompare
