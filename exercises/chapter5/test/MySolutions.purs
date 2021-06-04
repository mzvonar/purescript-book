module Test.MySolutions where

import Prelude
import Math (abs, pi, min, max) as Math
import Data.Maybe (Maybe(..))
import Data.Foldable (or)
import Data.Person (Person)
import Data.Picture hiding (shapeBounds)
import ChapterExamples (Amp(..), Volt(..))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k 
    | n < k     = 0
    | otherwise = (factorial n) / ( (factorial k) * (factorial (n - k)) )

pascal  :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person-> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton a _ = a

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin $ r * 2.0
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)
doubleScaleAndCenter (Line {x: x1, y: y1} {x: x2, y: y2}) = Line {x: origin.x - h, y: origin.y - v} {x: origin.x + h, y: origin.y + v}
    where
        h = Math.abs $ x2 - x1
        v = Math.abs $ y2 - y1
doubleScaleAndCenter (Text _ t) = Text origin t
doubleScaleAndCenter n = n

shapeText :: Shape -> Maybe String
shapeText (Text _ t) = Just t
shapeText _ = Nothing 


newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp i) (Volt v) = Watt (i * v)

area :: Shape -> Number
area (Circle _ r) = Math.pi * r * r
area (Rectangle _ w h) = w * h
area _ = 0.0

shapeBounds :: Shape -> Bounds
shapeBounds (Circle { x, y } r) =
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle { x, y } w h) =
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line p1 p2) =
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text { x, y } _) =
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped _ c w h) =
  { top:    c.y - h / 2.0
  , left:   c.x - w / 2.0
  , bottom: c.y + h / 2.0
  , right:  c.x + w / 2.0
  }