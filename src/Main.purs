module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Array (length) as Array
import Data.Traversable (traverse_)

import Game (tiles)
import Tile (Tile, WithRed(..))
import Game.Pictogram (picto)
import Game.Hint (hint)
import Game.Doc (doc)



{- instance OrdInt a => Ord a where
  compare a b = compare (position a) (position b) -}


main :: Effect Unit
main = do
  traverse_ log $ tileOneLiner <$> tiles NoRed
  log $ show $ Array.length $ tiles NoRed
  where
    tileOneLiner :: Tile -> String
    tileOneLiner tile = picto tile <> " | " <> hint tile <> " | " <> doc tile
