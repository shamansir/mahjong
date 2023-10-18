module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)


data Stick
  = Stick100
  | Stick1000
  | Stick5000
  | Stick10000


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
  | Suit6
  | Suit7
  | Suit8
  | Suit9


data Wind
  = Shau -- Shā / West 西
  | Nan -- South 南
  | Peu -- Pē / North 北
  | Ton  -- East 東


data Dragon
  = Hatsu -- 發 / Green
  | Chun -- 中 / Red
  | Haku -- 白 / White


data Season
  = Spring -- 春
  | Summer -- 夏
  | Autumn -- 秋
  | Winter -- 冬


data Flower
  = Plum -- 梅
  | Orchid -- 兰 or 蘭
  | Chrysanthenium -- 菊
  | Bamboo -- 竹


data Tile
  = Pinzu SuitValue -- Pin / p / Dots / 筒子 / ピン
  | Souzu SuitValue -- Soo / s / Bamboos / Bams, Sōzu / 索子 / ソウ
  | Manzu SuitValue -- Man / m / Characters / Cracks / 萬子 / 万子 / マン / ワン
  | Kaze Wind -- 風牌
  | Sangen Dragon -- 三元牌
  | Flower Flower -- Shikunshi / 四君子


data Kind
  = Terminal -- Rōtōhai
  | Honour -- Jihai / tsūpai / 字牌
  | Simple -- Chunchanpai


main :: Effect Unit
main = do
  log "🍝"
