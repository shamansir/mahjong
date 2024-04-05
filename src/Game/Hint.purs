module Game.Hint where


class Hint a where
    hint :: a -> String