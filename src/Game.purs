module Game where

import Prelude

import Data.Array as Array
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

import Dice (Dice(..))
import Stick (Stick(..))
import Tile (Tile(..), WithRed(..))
import Tile (dots, bamboos, characters, winds, dragons, flowers, seasons, reds) as Tile
import Wind (Wind(..), Prevalent(..))
import Player (Player)

import Game.Hand (Hand)
import Game.ByWind (ByWind)


data Phase
    = Pre
    | EastPhase Player
    | SouthPhase Player
    | Done


data Game = Game
    { hands :: ByWind (Hand /\ Array Stick)
    , deadWall :: Array Tile -- Vec tilesleft Tile
    , curTile :: Maybe Tile
    , dices :: Dice /\ Dice
    , phase :: Phase
    , players :: ByWind Player
    }


dices :: Array Dice
dices = [ Dice1, Dice1 ]


sticks :: Array Stick
sticks =
    Array.replicate 40 Stick100
    <> Array.replicate 37 Stick1000
    <> Array.replicate 8 Stick5000
    <> Array.replicate 4 Stick10000


tiles :: WithRed -> Array Tile
tiles wr =
    Array.concat $
       Array.replicate 4 (Set.toUnfoldable $ Tile.dots wr :: Array Tile)
    <> Array.replicate 4 (Set.toUnfoldable $ Tile.bamboos wr :: Array Tile)
    <> Array.replicate 4 (Set.toUnfoldable $ Tile.characters wr :: Array Tile)
    <> Array.replicate 4 (Set.toUnfoldable $ Tile.winds :: Array Tile)
    <> Array.replicate 4 (Set.toUnfoldable $ Tile.dragons :: Array Tile)


tilesEx :: WithRed -> Array Tile
tilesEx wr =
    tiles wr <> Array.concat
        (  Array.replicate 4 (Set.toUnfoldable $ Tile.flowers :: Array Tile)
        <> Array.replicate 4 (Set.toUnfoldable $ Tile.seasons :: Array Tile)
        )


reds :: Array Tile
reds = Set.toUnfoldable Tile.reds


flops :: Array Prevalent
flops = [ PrevalentEast ]