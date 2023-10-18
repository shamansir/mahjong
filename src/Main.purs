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
  = North
  | South
  | West
  | East


data Tile
  = Pinzu SuitValue -- Pin / p / Dots
  | Souzu SuitValue -- Soo / s / Bamboos / Bams, Sōzu
  | Manzu SuitValue -- Man / m / Characters / Cracks
  | Wind Wind


data Kind
  = Terminal -- Rōtōhai
  | Simple -- Chunchanpai


main :: Effect Unit
main = do
  log "🍝"
