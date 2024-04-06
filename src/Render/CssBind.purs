module Render.CssBind where

import Prelude

import Data.Newtype (class Newtype, wrap)

import Valued (valueOf)
import Tile (Tile(..), SuitValue(..))
import Wind (Wind(..))
import Dragon (Dragon(..))
import Flower (Flower(..))
import Season (Season(..))

newtype CssClass = CssClass String

derive instance Newtype CssClass _


class CssBind a where
    cssClass :: a -> CssClass


instance CssBind Tile where
  cssClass t = wrap $ "tile " <> case t of
    Dots dv -> "dots-" <> valueToSuffix dv
    Bamboo bv -> "bamboos-" <> valueToSuffix bv
    Character cv -> "chars-" <> valueToSuffix cv
    Wind w -> "wind-" <> windToSuffix w
    Dragon d -> "dragon-" <> dragonToSuffix d
    Flower f -> "flower-" <> flowerToSuffix f
    Season s -> "season-" <> seasonToSuffix s
    Jocker -> "jocker" -- ?
    where
      valueToSuffix = case _ of
        Value5Red -> "5-red"
        val -> show $ valueOf val
      windToSuffix = case _ of
        West -> "west"
        North -> "north"
        South -> "south"
        East -> "east"
      dragonToSuffix = case _ of
        White -> "white"
        Red -> "red"
        Green -> "green"
      flowerToSuffix = case _ of
        Plum -> "plum"
        Orchid -> "orchid"
        Chrysanthenium -> "chrysantenium"
        FBamboo -> "bamboo"
      seasonToSuffix = case _ of
        Winter -> "winter"
        Spring -> "spring"
        Summer -> "summer"
        Autumn -> "autumn"