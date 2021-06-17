module Test.MySolutions where

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (Reader, ReaderT, ask, local, mapReader, runReader, runReaderT)
import Data.Array (foldr, snoc)
import Data.Foldable (traverse_, foldM)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.String (Pattern(..), joinWith, stripPrefix)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Console (logShow)
import Split (Parser)
import Data.Newtype (unwrap)

-- sumArray :: Array Int -> State Int Unit
-- sumArray = traverse_ \n -> modify \sum -> sum + n

-- run = execState (do
--     sumArray [1, 2, 3]
--     sumArray [4, 5]
--     sumArray [6]) 0

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

sumArray :: Array Int -> State (Array Int) Unit
sumArray = traverse_ \n -> modify \sum -> sum `snoc` n

runt = do
    sumArray [1, 2, 3]
    sumArray [4, 5]
    sumArray [6]

runt' = 
    bind (sumArray [1, 2, 3]) (\_ -> 
        bind (sumArray [4, 5]) (\_ ->
            sumArray [6]
        )
    )

runt'' = 
    sumArray [1, 2, 3] >>= (\_ ->
        sumArray [4, 5] >>= (\_ ->
            sumArray [6]
        )
    )

runt''' = do
    _ <- sumArray [1, 2, 3]
    _ <- sumArray [4, 5]
    sumArray [6]

run = execState runt'' []


main = do
  logShow $ execState runt []
  logShow $ execState runt' []
  logShow $ execState runt'' []
  logShow $ execState runt''' []


safeDivide :: Number -> Number -> ExceptT String Identity Number
safeDivide n d = do
    case d of
        0.0 -> throwError "Division by zero"
        _ -> pure $ n / d
    

string :: String -> Parser String
string prefix = do
  s <- get
  lift $ tell ["The state is " <> s]
  case stripPrefix (Pattern prefix) s of
    Nothing -> lift $ lift $ throwError ["Could not parse"]
    Just suffix -> do
      put suffix
      pure prefix


-- type DocWriter = WriterT String (Reader Level) String
type DocWriter = ReaderT Level (WriterT (Array String) Identity) Unit

line' :: String -> DocWriter
line' s = do
    l <- ask
    tell $ [power "  " l <> s]

indent' :: DocWriter -> DocWriter
indent' = local $ (+) 1

render' :: DocWriter -> String
render' r = joinWith "\n" $ unwrap $ execWriterT $ runReaderT r 0