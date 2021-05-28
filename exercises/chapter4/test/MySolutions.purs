module Test.MySolutions where

import Prelude

import Data.Array (filter, head, length, null, tail, concatMap, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (lessThanOrEq)
import Math (pow)
import Data.Int (pow) as Int
import Test.Examples (factorsV3)
import Control.Alternative (guard)
import Data.Foldable
import Data.Path
import Debug

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
isPrime 0 = false
isPrime 1 = false
isPrime x = (length $ factorsV3 x) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  i <- a
  j <- b
  pure [i, j]


triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ square a + square b == square c
  pure [a, b, c]
    where
        square :: Int -> Int
        square = flip Int.pow 2


factorize :: Int -> Array Int
factorize 1 = [1]
factorize n = 
    if firstPrimeFactor == Nothing then [n]
    else concatMap factorize $ reverse $ fromMaybe ([] :: Array Int) firstPrimeFactor
    where
        firstPrimeFactor :: Maybe (Array Int)
        firstPrimeFactor = head $ fromMaybe [] $ tail $ factorsV3 n


allTrue :: Array Boolean -> Boolean
allTrue [] = false
allTrue x = foldl (\acc -> \a -> acc && a) true x

fibTailRec :: Int -> Int
fibTailRec n = fibTailRec' 0 1 0
    where
      fibTailRec' :: Int -> Int -> Int -> Int
      fibTailRec' a b c = 
        if c < n then
            fibTailRec' b (a + b) (c + 1) 
        else 
            a

reverse :: forall a. Array a -> Array a
reverse = foldl (flip (:)) []

onlyFiles :: Path -> Array Path
onlyFiles file = filter (not isDirectory) (allFiles file)
    where
        allFiles :: Path -> Array Path
        allFiles f = f : do 
            child <- ls f
            allFiles child

-- whereIs :: Path -> String -> Maybe Path
-- whereIs path search = 
--     if not null $ filter hasSearch files then
--         Just path
--     else
--         head childFindings
        
--     where
--         hasSearch :: Path -> Boolean
--         hasSearch c = filename c == search 

--         files = filter (not isDirectory) children
--         dirs = filter isDirectory children
--         children = ls path

--         childFindings = map (\d -> whereIs d search) dirs

whereIs :: Path -> String -> Maybe Path
whereIs path search = head $ whereIs' path $ ls path
    where
        whereIs' :: Path -> Array Path -> Array Path
        whereIs' base children = do
            child <- children
         
            if isDirectory child 
                then whereIs' child $ ls child
            else if filename child == filename base <> search
                then [base]
            else
                []