module Game.ByWind where

import Prelude

import Data.Vec (Vec, (+>))
import Data.Vec (empty) as Vec
import Data.Typelevel.Num (D4)
import Data.Tuple.Nested ((/\), type (/\))

import Wind (Wind(..))


type ByWind a =
    { south :: a, north :: a, west :: a, east :: a }


get :: forall a. Wind -> ByWind a -> a
get w bw = case w of
    South -> bw.south
    North -> bw.north
    West -> bw.west
    East -> bw.east


unfold :: forall a. ByWind a -> Vec D4 (Wind /\ a)-- Array (Wind /\ a)
unfold bw
    =  (North /\ bw.north)
    +> (West  /\ bw.west )
    +> (South /\ bw.south)
    +> (East  /\ bw.east )
    +> Vec.empty