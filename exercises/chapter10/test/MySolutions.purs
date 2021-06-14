module Test.MySolutions where

import Prelude

import Data.Function.Uncurried (Fn3)
import Data.Pair (Pair(..))
import Test.Examples (Complex, Quadratic)

-- Note to reader: Add your solutions to this file
foreign import volumeFn :: Fn3 Number Number Number Number

foreign import volumeArrow :: Number -> Number -> Number -> Number

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsImpl :: (Complex -> Complex -> Pair Complex) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair