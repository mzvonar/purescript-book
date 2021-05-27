module Test.MySolutions where

import Prelude

import Data.Array (filter, head, length, null, tail)
import Data.Maybe (fromMaybe)
import Data.Ord (lessThanOrEq)
import Math (pow)
import Test.Examples (factorsV3)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

countEven :: Array Int -> Int
countEven as = 
    if null as
        then 0
    else 
        (if (isEven $ fromMaybe 0 $ head as) then 1 else 0) + (countEven $ fromMaybe [] $ tail as)


squared :: Array Number -> Array Number
squared = map $ flip pow 2.0

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter $ lessThanOrEq 0.0

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite as = lessThanOrEq 0.0 <$?> as

isPrime :: Int -> Boolean
isPrime x = (length $ factorsV3 x) == 2
