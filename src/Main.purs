module Main where

import Prelude

import Effect (Effect)

import Data.Array (length) as Array
import Data.Traversable (traverse_)
import Data.String (joinWith) as String
import Data.Newtype (unwrap)

import Game (tiles, tilesEx, reds)
import Tile (Tile, WithRed(..))
import Game.Pictogram (picto)
import Game.Hint (hint)
import Game.Doc (doc)

import Render.CssBind (cssClass)

import Specular.Dom.Element (attr,  class_, dynText, el, onClick_, text, el')
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Ref (Ref)
import Specular.Ref as Ref


main :: Effect Unit
main = do
  -- | Will append widget to the body
  runMainWidgetInBody do
    counter :: Ref Int <- Ref.new 0
    -- | Subtract 1 from counter value
    let subtractCb = Ref.modify counter (add (negate 1))

    -- | Add 1 to counter value
    let addCb = Ref.modify counter (add 1)

    el "button" [class_ "btn", attr "type" "button", onClick_ addCb ] do
      text "+"

    dynText $ show <$> Ref.value counter

    el "button" [class_ "btn", attr "type" "button", onClick_ subtractCb ] do
      text "-"

    el "hr" [] $ pure unit

    el "div" [ class_ "tiles" ] do
      traverse_ tileImg $ tilesEx NoRed <> reds

    el "hr" [] $ pure unit

    el "textarea" [ class_ "list" ] do
      text $ String.joinWith "\n" $ tileOneLiner <$> tilesEx NoRed

    el "hr" [] $ pure unit

    el "span" [] do
      text $ show $ Array.length $ tiles NoRed

  where
    tileImg tile = el "span" [ class_ $ unwrap $ cssClass tile ] $ pure unit

    tileOneLiner :: Tile -> String
    tileOneLiner tile = picto tile <> " | " <> hint tile <> " | " <> doc tile