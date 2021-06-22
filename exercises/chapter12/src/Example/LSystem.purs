module Example.LSystem where

import Prelude

import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Graphics.Canvas (Context2D, closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, setShadowBlur, setShadowColor, setShadowOffsetX, setShadowOffsetY, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

lsystem :: forall a m s
         . Monad m
         => Array a
         -> (a -> Array a)
         -> (s -> a -> m s)
         -> Int
         -> s
         -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s i = go (concatMap prod s) (i - 1)

lsystem' :: forall a
          . Array a
          -> (a -> Array a)
          -> Int
          -> Array a
lsystem' init prod n = go init n
  where
    go s 0 = s
    go s i = go (concatMap prod s) (i - 1)

-- data Letter = L | R | F
type Angle = Number

data Letter = L Angle | R Angle | F Boolean

derive instance genericLetter :: Generic Letter _

instance showLetter :: Show Letter where
  show = genericShow

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

angle :: Number
angle = 60.0

initial :: Sentence
initial = [F false]

productions :: Letter -> Sentence
productions (L a) = [L a]
productions (R a) = [R a]
-- productions F = [F, L angle, F, R angle, R angle, F, L angle, F]
-- productions M = [F, L angle, F, R angle, R angle, F, L angle, F]
productions (F true) = [F true, L angle, F false, L angle, F true, R angle, F false, R angle, F true, R angle, F false, R angle, F true, L angle, F false, L angle, F true]
productions (F false) = [F false, R angle, F true, R angle, F false, L angle, F true, L angle, F false, L angle, F true, L angle, F false, R angle, F true, R angle, F false]

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

interpretLsystem :: forall a m s
                  . Monad m 
                  => Array a
                  -> (s -> a -> m s)
                  -> s
                  -> m s
interpretLsystem seq interpret state = foldM interpret state seq

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    interpret :: State -> Letter -> Effect State
    interpret state (L a) = pure $ state { theta = state.theta - Math.tau / (360.0 / a) }
    interpret state (R a) = pure $ state { theta = state.theta + Math.tau / (360.0 / a) }
    interpret state _ = do
      let x = state.x + Math.cos state.theta * 5.5
          y = state.y + Math.sin state.theta * 5.5
      
      lineTo ctx x y
      pure { x, y, theta: state.theta }

  setFillStyle ctx "#595"
  setStrokeStyle ctx "#000"

  let seq = lsystem' initial productions 3

  logShow seq
  -- let 
  --   setShadow :: Context2D -> Number -> Number -> Number -> String -> Effect Unit
  --   setShadow ctx x y b c = setShadowOffsetX ctx x $ setShadowOffsetY ctx y $ setShadowBlur ctx b $ setShadowColor ctx c
  
  setShadowOffsetX ctx 10.0
  setShadowOffsetY ctx 10.0
  setShadowBlur ctx 5.0
  setShadowColor ctx "#a92"

  strokePath ctx $ fillPath ctx $ do
    moveTo ctx initialState.x initialState.y
    -- _ <- lsystem initial productions interpret 5 initialState
    _ <- interpretLsystem seq interpret initialState
    closePath ctx