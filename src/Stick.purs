module Stick where

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


data Stick
  = Stick100 -- eight small dots
  | Stick1000 -- one red dot
  | Stick5000 -- one red dot surrounded by four small red dots
  | Stick10000 -- two black dots, between them one red dot surrounded by six small dots


instance HasPicto Stick where
  picto = case _ of
    Stick100 -> "🁫"
    Stick1000 -> "🂃"
    Stick5000 -> "🂋"
    Stick10000 -> "🂓"


sticks :: Set Stick
sticks = Set.fromFoldable [ Stick100, Stick1000, Stick5000, Stick10000 ]


instance Valued Stick where
  valueOf = case _ of
    Stick100 -> 100
    Stick1000 -> 1000
    Stick5000 -> 5000
    Stick10000 -> 10000


instance Hint Stick where
  hint = case _ of
    Stick100 -> "/100/"
    Stick1000 -> "/1000/"
    Stick5000 -> "/5000/"
    Stick10000 -> "/10000/"


instance Doc Stick where
  doc s = "Stick of value " <> show (valueOf s)


instance Batch Stick where
  all = sticks


instance Enc Stick where
  encode = hint


instance OrdInt Stick where position = valueOf
instance Eq Stick where eq = intEq
instance Ord Stick where compare = intCompare