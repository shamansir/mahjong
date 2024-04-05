module Dragon where

import Prelude

import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Order (class OrdInt, intEq, intCompare)
import Game.Hint (class Hint, hint)
import Game.Doc (class Doc)
import Game.Pictogram (class HasPicto)
import Game.Batch (class Batch)
import Game.Enc (class Enc)


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


instance HasPicto Dragon where
  picto = case _ of
    Green -> "🀅"
    Red -> "🀄"
    White -> "🀆"


instance Hint Dragon where
  hint = case _ of
    Green -> "<G>"
    Red -> "<R>"
    White -> "<W>"


instance Doc Dragon where
  doc = case _ of
    Green -> "Green Dragon; Hatsu / 發 / qingfa / Green"
    Red -> "Red Dragon; Chun / 中 / hongzhong / Red"
    White -> "White Dragon; Haku / 白 / baiban / White"


instance Batch Dragon where
  all = dragons


instance Enc Dragon where
  encode = hint


instance Eq Dragon where eq = intEq
instance Ord Dragon where compare = intCompare
