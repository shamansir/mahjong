module Render.CssBind where

import Prelude

import Data.Newtype (class Newtype, wrap)

import Valued (valueOf)
import Tile (Tile(..), Back(..), SuitValue(..))
import Wind (Wind(..), Prevalent(..))
import Dragon (Dragon(..))
import Flower (Flower(..))
import Season (Season(..))
import Stick (Stick(..))

newtype CssClass = CssClass String

derive instance Newtype CssClass _

data DesignSet
    = BlackWhite
    | Colored
    | DesignA
    -- | DesignB
    | DesignC
    | SepSVG
    | SepPNG


class CssBind a where
    cssClass :: a -> CssClass


instance CssBind DesignSet where
  cssClass ds = wrap $ "set-" <> case ds of
    BlackWhite -> "bw"
    Colored -> "colored"
    DesignA -> "des-a"
    -- DesignB -> "des-b"
    DesignC -> "des-c"
    SepSVG -> "sep-svg"
    SepPNG -> "sep-png"


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


instance CssBind Back where
  cssClass b = wrap $ "tile " <> case b of
    Back -> "back"


instance CssBind Prevalent where
  cssClass b = wrap $ "prev prev-" <> case b of
    PrevalentEast -> "east"
    PrevalentSouth -> "south"


instance CssBind Stick where
  cssClass b = wrap $ "stick stick-" <> case b of
    Stick100 -> "100"
    Stick1000 -> "1000"
    Stick5000 -> "5000"
    Stick10000 -> "10000"