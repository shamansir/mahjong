module Tile where

import Prelude


import Data.Set (Set)
import Data.Set (fromFoldable, toUnfoldable) as Set


import Order (class OrdInt, position, intEq, intCompare)
import Valued (class Valued, valueOf)


import Wind (Wind)
import Wind (winds) as W
import Dragon (Dragon)
import Dragon (dragons) as D
import Flower (Flower)
import Flower (flowers) as F
import Season (Season)
import Season (seasons) as S


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


data WithRed
    = WithRed
    | NoRed


data Tile
  = Dots SuitValue -- Chow / Pinzu / Pin / p / Dots / 筒子 / ピン
  | Bamboo SuitValue -- Pung / Souzu / Soo / s / Bamboos / Bams, Sōzu / 索子 / ソウ
  | Character SuitValue -- Kong / Manzu / Man / m / Characters / Cracks / 萬子 / 万子 / マン / ワン
  | Wind Wind -- Kaze / 風牌
  | Dragon Dragon -- Sangen / 三元牌
  | Flower Flower -- Shikunshi / 四君子
  | Season Season


valuesArr :: WithRed -> Array SuitValue
valuesArr = Set.toUnfoldable <<< values


values :: WithRed -> Set SuitValue
values = case _ of
    NoRed -> Set.fromFoldable [ Suit1, Suit2, Suit3, Suit4, Suit5, Suit6, Suit7, Suit8, Suit9 ]
    WithRed ->  Set.fromFoldable [ Suit1, Suit2, Suit3, Suit4, Suit5Red, Suit6, Suit7, Suit8, Suit9 ]


dots :: WithRed -> Set Tile
dots wr = Set.fromFoldable $ Dots <$> valuesArr wr


bamboos :: WithRed -> Set Tile
bamboos wr = Set.fromFoldable $ Bamboo <$> valuesArr wr


characters :: WithRed -> Set Tile
characters wr = Set.fromFoldable $ Character <$> valuesArr wr


winds :: Set Tile
winds = Set.fromFoldable $ Wind <$> (Set.toUnfoldable W.winds :: Array Wind)


dragons :: Set Tile
dragons = Set.fromFoldable $ Dragon <$> (Set.toUnfoldable D.dragons :: Array Dragon)


flowers :: Set Tile
flowers = Set.fromFoldable $ Flower <$> (Set.toUnfoldable F.flowers :: Array Flower)


seasons :: Set Tile
seasons = Set.fromFoldable $ Season <$> (Set.toUnfoldable S.seasons :: Array Season)


allTiles :: WithRed -> Set Tile
allTiles wr = dots wr <> bamboos wr <> characters wr <> winds <> dragons


allTiles' :: WithRed -> Set Tile
allTiles' wr = dots wr <> bamboos wr <> characters wr <> winds <> dragons <> flowers


allTiles'' :: WithRed -> Set Tile
allTiles'' wr = dots wr <> bamboos wr <> characters wr <> winds <> dragons <> flowers <> seasons


instance Eq Tile where eq = intEq
instance Ord Tile where compare = intCompare


instance OrdInt Tile where
  position = case _ of
    Dots suit -> 10 + position suit
    Bamboo suit -> 20 + position suit
    Character suit -> 30 + position suit
    Wind wind -> 40 + position wind
    Dragon dragon -> 50 + position dragon
    Flower flower -> 60 + position flower
    Season season -> 70 + position season


data Kind
  = Terminal -- Rōtōhai
  | Honour -- Jihai / tsūpai / 字牌 : Four Winds and Three Dragons
  | Simple -- Chunchanpai : Suits + Values
