module Test.MySolutions where

import Data.Hashable
import Prelude

import Control.Extend (class Extend)
import Data.Array (foldl, foldr, foldMap, nubEq, nub, nubByEq, length) as Array
import Data.Foldable (class Foldable, foldMap, fold, foldl, foldr, maximum)
import Data.Formatter.Internal (repeat)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, wrap, over2)
import Data.Ord (class Ord1)
import Data.Semigroup (append)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Hours)
import Prim.Ordering (LT)

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

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty a arr) = "NonEmpty" <> show a <> " " <> show arr

derive instance eqNonEmpty :: Eq a => Eq (NonEmpty a)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a arr1) (NonEmpty b arr2) = NonEmpty a (arr1 <> [b] <> arr2)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a arr) = NonEmpty (f a) (map f arr)

data Extended a = Infinite | Finite a

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite a) (Finite b) = eq a b
  eq _ _ = false
  

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite b) = compare a b

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f z (NonEmpty a arr) = f a (Array.foldr f z arr) 
  foldl f z (NonEmpty a arr) = Array.foldl f (f z a) arr
  foldMap f (NonEmpty a arr) = f a <> Array.foldMap f arr

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr g z (OneMore a f) = g a (foldr g z f) 
  foldl g z (OneMore a f) = foldl g (g z a) f
  foldMap g (OneMore a f) = g a <> foldMap g f

derive newtype instance eqPoint :: Eq Point
derive newtype instance ordPoint :: Ord Point

-- instance eqShape :: Eq Shape where
--   eq (Circle c1 r1) (Circle c2 r2) = c1 == c2 && r1 == r2
--   eq (Rectangle c1 w1 h1) (Rectangle c2 w2 h2) = c1 == c2 && w1 == w2 && h1 == h2
--   eq (Line s1 e1) (Line s2 e2) = s1 == s2 && e1 == e2
--   eq (Text p1 s1) (Text p2 s2) = p1 == p2 && s1 == s2
--   eq _ _ = false

derive instance eqShape :: Eq Shape
derive instance ordShape :: Ord Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = Array.nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = Array.nub


unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum = fromJust <<< maximum 

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

derive newtype instance showMultiply :: Show Multiply
derive newtype instance eqMultiply :: Eq Multiply

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act m a = getValue (m <> (Multiply a))
    where
      getValue (Multiply a) = a

instance actionMultiplyString :: Action Multiply String where
  act m a = repeat a $ getValue m
    where
      getValue (Multiply a) = a

instance actionArray :: Action m a => Action m (Array a) where
  act m as = map (act m) as 

newtype Self m = Self m

-- derive instance newtypeSelf :: Newtype Self _
-- derive instance genericSelf :: Generic Self _

-- instance showSelf :: Show Self where
--   show = genericShow

derive newtype instance showSelf :: Show m => Show (Self m)
derive newtype instance eqSelf :: Eq m => Eq (Self m)

instance actionSelf :: Monoid m => Action m (Self m) where
  act m (Self a) = Self (m <> a)


arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
-- arrayHasDuplicates a = (Array.length a) `not eq` (Array.length $ Array.nubByEq (\a b -> (hash a) == (hash b) && a == b) a)
arrayHasDuplicates a = ((not eq) `on` Array.length) a (Array.nubByEq (\a b -> (hash a) == (hash b) && a == b) a)

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
  hash (Hour h) = hashCode $ h `mod` 12