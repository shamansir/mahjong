module Game.Batch where

import Prelude


import Data.Set (Set)


class Batch a where
    all :: Set a