module Dragon where

import Prelude

import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)


data Dragon
  = Green -- Hatsu / 發 / Green
  | Red -- Chun / 中 / Red
  | White -- Haku / 白 / White


dragons :: Set Dragon
dragons = Set.fromFoldable [ White, Green, Red ]


instance OrdInt Dragon where
  position = case _ of
    White -> 0
    Green -> 1
    Red -> 2


instance Eq Dragon where eq = intEq
instance Ord Dragon where compare = intCompare