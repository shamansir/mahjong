module Game.Hand where

import Prelude

import Data.Vec (Vec)
import Data.Typelevel.Num (D13)

import Tile (Tile(..))


type Hand = Vec D13 Tile