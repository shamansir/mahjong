module Game.Enc where

import Prelude

import Data.Maybe (Maybe(..))

import Game.Batch (class Batch)


class Batch a <= Enc a where
    encode :: a -> String -- must be unique


decode :: forall a. Enc a => String -> Maybe a
decode = const Nothing  -- TODO!