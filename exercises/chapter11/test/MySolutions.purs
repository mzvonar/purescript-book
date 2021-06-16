module Test.MySolutions where

import Control.Monad.State
import Control.Monad.State.Class
import Prelude

import Control.Monad.Reader (Reader, ask, local, mapReader, runReader)
import Data.Array (foldr)
import Data.Foldable (traverse_, foldM)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.String (joinWith)
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Data.Tuple (Tuple(..))

sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

run = execState (do
    sumArray [1, 2, 3]
    sumArray [4, 5]
    sumArray [6]) 0

testParens :: String -> Boolean
testParens s = 0 == execState (countParens $ toCharArray s) 0
    where
        countParens :: Array Char -> State Int Unit
        countParens = traverse_ \c -> modify \open -> 
            case c of
                '(' -> open + 1
                ')' | open > 0  -> open - 1
                    | otherwise -> open + 1
                _   -> open
    

type Level = Int

type Doc = Reader Level String

line :: String -> Doc
line s = do
    l <- ask
    pure $ power " " l <> s

indent :: Doc -> Doc
indent = local \l -> l + 2

cat :: Array Doc -> Doc
cat as = mapReader (joinWith "\n") $ sequence as

render :: Doc -> String
render r = runReader r 0

create = render $ cat
  [ line "Here is some indented text:"
  , indent $ cat
      [ line "I am indented"
      , line "So am I"
      , indent $ line "I am even more indented"
      ]
  ]

-- create = render $ cat
--   [ line "Here is some indented text:"
--   , line "Nope"
--   ]

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \a -> do
  tell $ Additive a
  pure unit

collatz :: Int -> Tuple Int (Array Int)
collatz n = runWriter $ go n 0
    where
        go :: Int -> Int -> Writer (Array Int) Int
        go 1 c = do
            tell [1]
            pure c
        go n c | n `mod` 2 == 0 = do
            tell [n]
            go (n / 2) (c + 1)
        go n c = do
            tell [n]
            go (3 * n + 1) (c + 1)