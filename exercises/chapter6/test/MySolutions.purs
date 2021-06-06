module Test.MySolutions where

import Prelude

import Data.Newtype (class Newtype, wrap, over2)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- Note to reader: Add your solutions to this file
newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
    show (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"
  
newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
    show (Complex {real, imaginary}) = show real <> (if imaginary < 0.0 then "" else "+") <> show imaginary <> "i"

derive newtype instance eqComplex :: Eq Complex

derive instance newtypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
    -- zero = wrap {real: 0.0, imaginary: 0.0}
    zero = wrap zero
    -- add (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = wrap {real: r1 + r2, imaginary: i1 + i2}
    add = over2 Complex (+)
    mul (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = 
      Complex { real: r1 * r2 - i1 * i2, imaginary: (r1 * i2) + (i1 * r2)}
    one = wrap {real: 1.0, imaginary: 0.0}
    
derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow