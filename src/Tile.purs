module Tile where

import Prelude


import Order (class OrdInt, position, intEq, intCompare)
import Valued (class Valued, valueOf)


import Wind (Wind)
import Dragon (Dragon)


data SuitValue
  = Suit1
  | Suit2
  | Suit3
  | Suit4
  | Suit5
  | Suit5Red
  | Suit6
  | Suit7
  | Suit8
  | Suit9


instance Valued SuitValue where
  valueOf = case _ of
    Suit1 -> 1
    Suit2 -> 2
    Suit3 -> 3
    Suit4 -> 4
    Suit5 -> 5
    Suit5Red -> 5
    Suit6 -> 6
    Suit7 -> 7
    Suit8 -> 8
    Suit9 -> 9


instance OrdInt SuitValue where position = valueOf
instance Eq SuitValue where eq = intEq
instance Ord SuitValue where compare = intCompare


{-
data Season
  = Spring -- 春
  | Summer -- 夏
  | Autumn -- 秋
  | Winter -- 冬
-}

{-
data Flower
  = Plum -- 梅
  | Orchid -- 兰 or 蘭
  | Chrysanthenium -- 菊
  | FBamboo -- 竹
-}


data Tile
  = Dots SuitValue -- Chow / Pinzu / Pin / p / Dots / 筒子 / ピン
  | Bamboo SuitValue -- Pung / Souzu / Soo / s / Bamboos / Bams, Sōzu / 索子 / ソウ
  | Character SuitValue -- Kong / Manzu / Man / m / Characters / Cracks / 萬子 / 万子 / マン / ワン
  | Wind Wind -- Kaze / 風牌
  | Dragon Dragon -- Sangen / 三元牌
  -- | Flower Flower -- Shikunshi / 四君子


instance OrdInt Tile where
  position = case _ of
    Dots suit -> 10 + position suit
    Bamboo suit -> 20 + position suit
    Character suit -> 30 + position suit
    Wind wind -> 40 + position wind
    Dragon dragon -> 50 + position dragon


data Kind
  = Terminal -- Rōtōhai
  | Honour -- Jihai / tsūpai / 字牌 : Four Winds and Three Dragons
  | Simple -- Chunchanpai : Suits + Values
