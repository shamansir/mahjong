module Stick where

import Prelude


import Data.Set (Set)
import Data.Set (fromFoldable) as Set


import Order (class OrdInt, intEq, intCompare)
import Valued (class Valued, valueOf)


data Stick
  = Stick100 -- eight small dots
  | Stick1000 -- one red dot
  | Stick5000 -- one red dot surrounded by four small red dots
  | Stick10000 -- two black dots, between them one red dot surrounded by six small dots


sticks :: Set Stick
sticks = Set.fromFoldable [ Stick100, Stick1000, Stick5000, Stick10000 ]


instance Valued Stick where
  valueOf = case _ of
    Stick100 -> 100
    Stick1000 -> 1000
    Stick5000 -> 5000
    Stick10000 -> 10000


instance OrdInt Stick where position = valueOf
instance Eq Stick where eq = intEq
instance Ord Stick where compare = intCompare