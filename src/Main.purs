module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)


data Stick
  = Stick100 -- eight small dots
  | Stick1000 -- one red dot
  | Stick5000 -- one red dot surrounded by four small red dots
  | Stick10000 -- two black dots, between them one red dot surrounded by six small dots


data Dice
  = Dice1
  | Dice2
  | Dice3
  | Dice4
  | Dice5
  | Dice6


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


data Wind
  = West -- Shau / Shā / West 西
  | South -- Nan / South 南
  | North -- Peu / Pē / North 北
  | East  -- Ton / East / 東


data Dragon
  = Hatsu -- Hatsu / 發 / Green
  | Red -- Chun / 中 / Red
  | White -- Haku / 白 / White


data Season
  = Spring -- 春
  | Summer -- 夏
  | Autumn -- 秋
  | Winter -- 冬


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


data Kind
  = Terminal -- Rōtōhai
  | Honour -- Jihai / tsūpai / 字牌 : Four Winds and Three Dragons
  | Simple -- Chunchanpai : Suits + Values


class Valued a where
  valueOf :: a -> Int


class OrdInt a where
  position :: a -> Int


instance OrdInt Wind where
  position = case _ of
    East -> 0
    South -> 1
    West -> 2
    North -> 3


instance Eq Wind where
  eq = intEq


instance Ord Wind where
  compare = intCompare


{- instance OrdInt a => Ord a where
  compare a b = compare (position a) (position b) -}


intEq :: forall a. OrdInt a => a -> a -> Boolean
intEq a b = eq (position a) (position b)


intCompare :: forall a. OrdInt a => a -> a -> Ordering
intCompare a b = compare (position a) (position b)


main :: Effect Unit
main = do
  log "🍝"
