module Example.Rectangle where

import Prelude

import Data.Array (uncons, (..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow, log)
import Graphics.Canvas (Context2D, arc, closePath, fillPath, strokePath, 
                        getCanvasElementById, getContext2D, lineTo, moveTo, rect, 
                        setFillStyle, setStrokeStyle, withContext, rotate, scale,
                        translate)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (origin)

type Point = { x :: Number, y :: Number }

renderPath
  :: Context2D
  -> Array Point
  -> Effect Unit
renderPath _ [] = pure unit
renderPath ctx points = do
  case uncons points of
    Just {head, tail} -> do
      moveTo ctx head.x head.y
      for_ tail \p -> lineTo ctx p.x p.y
      closePath ctx
    Nothing -> pure unit
  
renderPath_ :: Context2D -> Array Point -> Effect Unit
renderPath_ ctx p = fillPath ctx $ strokePath ctx $ renderPath ctx p

renderF :: Context2D -> Array Number -> (Number -> Point) -> Effect Unit
renderF ctx ns f = renderPath_ ctx $ map f ns

floatRange :: Int -> Int -> Array Number
floatRange s e = map (Int.toNumber >>> \n -> n / 10.0) $ s .. e

toSize :: Number -> Number
toSize = (*) 600.0

linear :: Number -> Point
linear n = {x: toSize (n), y: toSize (n)}

exp :: Number -> Point
exp n = {x: toSize (n/2.0), y: toSize (n/2.0)}

rotateAround :: forall a. Context2D -> Point -> Effect a -> Effect a
rotateAround ctx {x, y} a = withContext ctx do
  moveTo ctx x y
  translate ctx { translateX: x, translateY: y}
  -- scale ctx { scaleX: 2.0, scaleY: 2.0 }
  rotate ctx $ Math.tau / 2.0
  translate ctx { translateX: -x, translateY: -y}
  a


main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"
  setStrokeStyle ctx "#000"

  rotateAround ctx {x: 250.0, y: 250.0} $ fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  -- fillPath ctx do
  --   rect ctx
  --     { x: 50.0
  --     , y: 250.0
  --     , width: 100.0
  --     , height: 100.0
  --     }

  --   rect ctx
  --     { x: 200.0
  --     , y: 250.0
  --     , width: 100.0
  --     , height: 100.0
  --     }
  -- fillPath ctx do
  --   arc ctx
  --     { x: 250.0
  --     , y: 250.0
  --     , radius: 50.0
  --     , start: 0.0
  --     , end: Math.tau * 2.0 / 6.0
  --     }

  --   lineTo ctx 250.0 250.0
  -- renderPath_ ctx 
  --   [{x: 50.0, y: 50.0}
  --   ,{x: 100.0, y: 50.0}
  --   ,{x: 100.0, y: 100.0}
  --   -- ,{x: 50.0, y: 100.0}
  --   ]

  -- let range = floatRange 0 10
  -- let f = exp
  -- log $ "Range: " <> show range
  -- log $ "Linear: " <> (show $ map linear range)
  -- log $ "Exp: " <> (show $ map f range)
  -- renderF ctx range linear

  -- setStrokeStyle ctx "#f00"
  -- renderF ctx range f
