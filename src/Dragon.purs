module Dragon where

import Prelude


import Order (class OrdInt, intEq, intCompare)


data Dragon
  = Green -- Hatsu / 發 / Green
  | Red -- Chun / 中 / Red
  | White -- Haku / 白 / White


instance OrdInt Dragon where
  position = case _ of
    White -> 0
    Green -> 1
    Red -> 2


instance Eq Dragon where eq = intEq
instance Ord Dragon where compare = intCompare