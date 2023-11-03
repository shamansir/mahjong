module Game where

import Prelude

import Data.Array as Array
import Data.Set as Set

import Dice (Dice)
import Stick (Stick(..))
import Tile (Tile(..), WithRed(..))
import Tile (dots, bamboos, characters, winds) as Tile
import Wind (Wind(..), Prevalent(..))


dices :: Array Dice
dices = []


sticks :: Array Stick
sticks = []


tiles :: WithRed -> Array Tile
tiles wr =
    Array.concat $
       Array.replicate 4 (Set.toUnfoldable $ Tile.dots wr :: Array Tile)
    <> Array.replicate 4 (Set.toUnfoldable $ Tile.bamboos wr :: Array Tile)
    <> Array.replicate 4 (Set.toUnfoldable $ Tile.characters wr :: Array Tile)


flops :: Array Prevalent
flops = []