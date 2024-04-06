module Tile where

import Prelude


import Data.Set (Set)
import Data.Set (fromFoldable, toUnfoldable) as Set
import Data.Newtype (wrap)

import Order (class OrdInt, position, intEq, intCompare)
import Valued (class Valued, valueOf)

import Game.Hint (class Hint, hint)
import Game.Doc (class Doc, doc)
import Game.Pictogram (class HasPicto, picto)
import Game.Batch (class Batch)
import Game.Enc (class Enc)


import Wind (Wind)
import Wind (winds) as W
import Dragon (Dragon)
import Dragon (dragons) as D
import Flower (Flower)
import Flower (flowers) as F
import Season (Season)
import Season (seasons) as S


data SuitValue
  = Value1
  | Value2
  | Value3
  | Value4
  | Value5
  | Value5Red
  | Value6
  | Value7
  | Value8
  | Value9


instance Valued SuitValue where
  valueOf = case _ of
    Value1 -> 1
    Value2 -> 2
    Value3 -> 3
    Value4 -> 4
    Value5 -> 5
    Value5Red -> 5
    Value6 -> 6
    Value7 -> 7
    Value8 -> 8
    Value9 -> 9


instance Hint SuitValue where
  hint = case _ of
    Value1 -> ":1:"
    Value2 -> ":2:"
    Value3 -> ":3:"
    Value4 -> ":4:"
    Value5 -> ":5:"
    Value5Red -> ":5:"
    Value6 -> ":6:"
    Value7 -> ":7:"
    Value8 -> ":8:"
    Value9 -> ":9:"


instance OrdInt SuitValue where position = valueOf
instance Eq SuitValue where eq = intEq
instance Ord SuitValue where compare = intCompare


data WithRed
    = WithRed
    | NoRed


data Suit
  = DotsS
  | BambooS
  | CharacterS
  | WindS
  | DragonS
  | FlowerS
  | SeasonS
  | JockerS


instance Hint Suit where
  hint = case _ of
    DotsS -> "*.*"
    BambooS -> "*B*"
    CharacterS -> "*C*"
    WindS -> "*W*"
    DragonS -> "*D*"
    FlowerS -> "*F*"
    SeasonS -> "*S*"
    JockerS -> "*J*"


instance Doc Suit where
  doc = case _ of
    DotsS -> "Dots; Chow / Pinzu / Pin / p / Dots / 筒子 / ピン"
    BambooS -> "Bamboo; Pung / Souzu / Soo / s / Bamboos / Bams, Sōzu / 索子 / ソウ"
    CharacterS -> "Character; Kong / Manzu / Man / m / Characters / Cracks / 萬子 / 万子 / マン / ワン"
    WindS -> "Wind; Kaze / 風牌"
    DragonS -> "Dragon; Sangen / 三元牌"
    FlowerS -> "Flower; Shikunshi / 四君子"
    SeasonS -> "Season;"
    JockerS -> "Jocker; Baida"


data Tile
  = Dots SuitValue -- Chow / Pinzu / Pin / p / Dots / 筒子 / ピン
  | Bamboo SuitValue -- Pung / Souzu / Soo / s / Bamboos / Bams, Sōzu / 索子 / ソウ
  | Character SuitValue -- Kong / Manzu / Man / m / Characters / Cracks / 萬子 / 万子 / マン / ワン
  | Wind Wind -- Kaze / 風牌
  | Dragon Dragon -- Sangen / 三元牌
  | Flower Flower -- Shikunshi / 四君子
  | Season Season
  | Jocker -- Baida
--   | Back


instance HasPicto Tile where
  picto = case _ of
    Dots Value1 -> "🀙"
    Dots Value2 -> "🀚"
    Dots Value3 -> "🀛"
    Dots Value4 -> "🀜"
    Dots Value5 -> "🀝"
    Dots Value5Red -> "🀝"
    Dots Value6 -> "🀞"
    Dots Value7 -> "🀟"
    Dots Value8 -> "🀠"
    Dots Value9 -> "🀡"
    Bamboo Value1 -> "🀐"
    Bamboo Value2 -> "🀑"
    Bamboo Value3 -> "🀒"
    Bamboo Value4 -> "🀓"
    Bamboo Value5 -> "🀔"
    Bamboo Value5Red -> "🀔"
    Bamboo Value6 -> "🀕"
    Bamboo Value7 -> "🀖"
    Bamboo Value8 -> "🀗"
    Bamboo Value9 -> "🀘"
    Character Value1 -> "🀇"
    Character Value2 -> "🀈"
    Character Value3 -> "🀉"
    Character Value4 -> "🀊"
    Character Value5 -> "🀋"
    Character Value5Red -> "🀋"
    Character Value6 -> "🀌"
    Character Value7 -> "🀍"
    Character Value8 -> "🀎"
    Character Value9 -> "🀏"
    Wind wind -> picto wind
    Dragon dragon -> picto dragon
    Flower flower -> picto flower
    Season season -> picto season
    Jocker -> "🀪"
    -- Back -> "🀫"



instance Hint Tile where
  hint = case _ of
    Dots sv -> "+D" <> show (valueOf sv) <> "+"
    Bamboo sv -> "+B" <> show (valueOf sv) <> "+"
    Character sv -> "+C" <> show (valueOf sv) <> "+"
    Wind w -> hint w
    Dragon d -> hint d
    Flower f -> hint f
    Season s -> hint s
    Jocker -> "+J+"


instance Doc Tile where
  doc = case _ of
    Dots sv -> "Dots (筒子 / ピン) of value " <> show (valueOf sv)
    Bamboo sv -> "Bamboos (索子 / ソウ) of value " <> show (valueOf sv)
    Character sv -> "Characters (萬子 / マン) of value " <> show (valueOf sv)
    Wind w -> doc w
    Dragon d -> doc d
    Flower f -> doc f
    Season s -> doc s
    Jocker -> "Jocker"


suitOf :: Tile -> Suit
suitOf = case _ of
  Dots _ -> DotsS
  Bamboo _ -> BambooS
  Character _ -> CharacterS
  Wind _ -> WindS
  Dragon _ -> DragonS
  Flower _ -> FlowerS
  Season _ -> SeasonS
  Jocker -> JockerS


valuesArr :: WithRed -> Array SuitValue
valuesArr = Set.toUnfoldable <<< values


values :: WithRed -> Set SuitValue
values = case _ of
    NoRed -> Set.fromFoldable [ Value1, Value2, Value3, Value4, Value5, Value6, Value7, Value8, Value9 ]
    WithRed ->  Set.fromFoldable [ Value1, Value2, Value3, Value4, Value5Red, Value6, Value7, Value8, Value9 ]


reds :: Set Tile
reds = Set.fromFoldable [ Dots Value5Red, Bamboo Value5Red, Character Value5Red ]


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


instance Batch Tile where
  all = allTiles'' NoRed


instance Enc Tile where
  encode = hint


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
    Jocker -> 80
    -- Back -> 90


data Kind
  = Terminal -- Rōtōhai
  | Honour -- Jihai / tsūpai / 字牌 : Four Winds and Three Dragons
  | Simple -- Chunchanpai : Suits + Values


instance Doc Kind where
  doc = case _ of
    Terminal -> "Teminal; Rōtōhai"
    Honour -> "Honour; Jihai / tsūpai / 字牌 : Four Winds and Three Dragons"
    Simple -> "Simple; Chunchanpai : Suits + Values"


-- FIXME : implement properly
kindOf :: Tile -> Kind
kindOf = case _ of
  Dots Value1 -> Terminal
  Dots Value9 -> Terminal
  Bamboo Value1 -> Terminal
  Bamboo Value9 -> Terminal
  Character Value1 -> Terminal
  Character Value9 -> Terminal
  Dots _ -> Simple
  Bamboo _ -> Simple
  Character _ -> Simple
  Wind _ -> Honour
  Dragon _ -> Honour
  Flower _ -> Honour -- ?
  Season _ -> Honour -- ?
  Jocker -> Honour -- ?