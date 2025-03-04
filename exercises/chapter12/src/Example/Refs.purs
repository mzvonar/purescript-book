module Example.Refs where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, arc, fillPath, getCanvasElementById, getContext2D, rect, rotate, scale, setFillStyle, strokePath, translate, withContext)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

type Point = { x :: Number, y :: Number }

strokeAndFill :: forall a. Context2D -> Effect a -> Effect a
strokeAndFill ctx a = fillPath ctx $ strokePath ctx a

render :: Context2D -> Int -> Effect Unit
render ctx count = void do
  setFillStyle ctx "#FFF"

  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 600.0
    , height: 600.0
    }

  setFillStyle ctx "#0F0"

  withContext ctx do
    let scaleX = Math.sin (toNumber count * Math.tau / 8.0) + 1.5
    let scaleY = Math.sin (toNumber count * Math.tau / 12.0) + 1.5

    translate ctx { translateX: 300.0, translateY:  300.0 }
    rotate ctx (toNumber count * Math.tau / 36.0)
    scale ctx { scaleX: scaleX, scaleY: scaleY }
    translate ctx { translateX: -100.0, translateY: -100.0 }

    strokeAndFill ctx $ rect ctx
      { x: 0.0
      , y: 0.0
      , width: 200.0
      , height: 200.0
      }

render' :: Context2D -> Effect Unit
render' ctx = do
  setFillStyle ctx "#FFF"

  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 600.0
    , height: 600.0
    }

  setFillStyle ctx "#0F0"

  x <- random
  y <- random
  r <- random
  strokeAndFill ctx $ arc ctx
    { x     : x * 600.0
    , y     : y * 600.0
    , radius: r * 150.0
    , start : 0.0
    , end   : Math.tau
    }

rotateAround :: forall a. Context2D -> Point -> Number -> Effect a -> Effect a
rotateAround ctx {x, y} count a = withContext ctx do
  translate ctx { translateX: x, translateY: y}
  -- scale ctx { scaleX: count, scaleY: count }
  rotate ctx $ count * Math.tau / 36.0
  translate ctx { translateX: -x, translateY: -y}
  a

render'' :: Context2D -> Int -> Effect Unit
render'' ctx count = do
  setFillStyle ctx "#FFF"

  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 600.0
    , height: 600.0
    }

  setFillStyle ctx "#0F0"

  rotateAround ctx {x: 300.0, y: 300.0} (toNumber count) $ do
    strokeAndFill ctx $ rect ctx
      { x: 250.0
      , y: 250.0
      , width: 100.0
      , height: 100.0
      }
  

-- main :: Effect Unit
-- main = void $ unsafePartial do
--   Just canvas <- getCanvasElementById "canvas"
--   ctx <- getContext2D canvas

--   clickCount <- Ref.new 0

--   render ctx 0
--   doc <- map (toParentNode <<< toDocument) (document =<< window)
--   Just node <- querySelector (QuerySelector "#canvas") doc

--   clickListener <- eventListener $ \_ -> do
--     logShow "Mouse clicked!"
--     count <- Ref.modify (\count -> count + 1) clickCount
--     render ctx count

--   addEventListener (EventType "click") clickListener true (toEventTarget node)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  clickCount <- Ref.new 0

  render'' ctx 0
  doc <- map (toParentNode <<< toDocument) (document =<< window)
  Just node <- querySelector (QuerySelector "#canvas") doc

  clickListener <- eventListener $ \_ -> do 
    logShow "Mouse clicked!"
    count <- Ref.modify (\count -> count + 1) clickCount
    render'' ctx count

  addEventListener (EventType "click") clickListener true (toEventTarget node)