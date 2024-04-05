module Game.Doc where

import Prelude


class Doc a where
    doc :: a -> String