module Game.Action where

import Prelude


import Game.Steal (Steal)


data Action
    = Steal Steal
    | Tsumo
    | Riichi