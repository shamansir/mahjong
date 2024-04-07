module Main where

import Prelude

import Effect (Effect)

import Data.Array (length) as Array
import Data.Traversable (traverse_)
import Data.String (joinWith) as String
import Data.Newtype (unwrap)

import Game (tiles, tilesEx, reds)
import Tile (Tile, Back(..), WithRed(..))
import Wind (Prevalent(..))
import Stick (Stick(..))
import Game.Pictogram (picto)
import Game.Hint (hint)
import Game.Doc (doc)

import Render.CssBind (cssClass, DesignSet(..))

import Specular.Dom.Element (attr,  class_, dynText, el, onClick_, text, el', classesD)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Ref (Ref)
import Specular.Ref as Ref


main :: Effect Unit
main = do
  -- | Will append widget to the body
  runMainWidgetInBody do
    tileDesign :: Ref DesignSet <- Ref.new SepPNG
    thingsDesign :: Ref DesignSet <- Ref.new DesignA
    -- | Subtract 1 from counter value
    let curTileDesign = Ref.value tileDesign
    let curThingsDesign = Ref.value thingsDesign
    let setTileDesign = Ref.modify tileDesign <<< const
    let setThingsDesign = Ref.modify thingsDesign <<< const

    -- | Add 1 to counter value

    el "div" [ ] do
      traverse_ (\d -> designButton (setTileDesign d) (designName d)) $ [ Colored, SepSVG, SepPNG, BlackWhite, DesignA, DesignC ]
    br


    text "Tiles: "
    br

    dynText $ designName <$> curTileDesign
    br

    hr

    el "div" [ classesD (toTilesClasses <$> curTileDesign) ] do
      traverse_ tileImg $ tilesEx NoRed <> reds
      el "span" [ class_ $ unwrap $ cssClass Back ] $ pure unit

    hr

    text "Things: "
    br
    dynText $ designName <$> curThingsDesign
    br

    el "div" [ ] do
      traverse_ (\d -> designButton (setThingsDesign d) (designName d)) $ [ DesignA, DesignC ]

    el "div" [ classesD (toThingsClasses <$> curThingsDesign) ] do
      el "span" [ class_ $ unwrap $ cssClass PrevalentEast ] $ pure unit
      el "span" [ class_ $ unwrap $ cssClass PrevalentSouth ] $ pure unit
      el "span" [ class_ $ unwrap $ cssClass Stick100 ] $ pure unit
      el "span" [ class_ $ unwrap $ cssClass Stick1000 ] $ pure unit
      el "span" [ class_ $ unwrap $ cssClass Stick5000 ] $ pure unit
      el "span" [ class_ $ unwrap $ cssClass Stick10000 ] $ pure unit

    hr

    el "textarea" [ class_ "list" ] do
      text $ String.joinWith "\n" $ tileOneLiner <$> tilesEx NoRed

    hr

    el "span" [] do
      text $ show $ Array.length $ tiles NoRed

  where
    br = el "br" [] $ pure unit
    hr = el "hr" [] $ pure unit
    toTilesClasses cs = [ "tiles", unwrap $ cssClass cs ]
    toThingsClasses cs = [ "tiles", unwrap $ cssClass cs ]
    designName = case _ of
      Colored -> "Colored"
      SepSVG -> "Sep. SVG"
      SepPNG -> "Sep. PNG"
      BlackWhite -> "Black & White"
      DesignA -> "Design A"
      DesignC -> "Design C"
    designButton ds name = el "button" [class_ "btn", attr "type" "button", onClick_ ds ] do text name

    tileImg tile = el "span" [ class_ $ unwrap $ cssClass tile ] $ pure unit

    tileOneLiner :: Tile -> String
    tileOneLiner tile = picto tile <> " | " <> hint tile <> " | " <> doc tile