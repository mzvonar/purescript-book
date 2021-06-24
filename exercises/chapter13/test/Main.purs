module Test.Main where

import Data.Maybe.First
import Prelude

import Byte (Byte(..), idByte, unwrapByte)
import Control.Monad.Except (except)
import Data.Array (sort, sortBy, cons, range)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Char (toCharCode, fromCharCode)
import Data.Foldable (foldr, foldl, all, foldMap)
import Data.Function (on, applyN)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (greaterThanOrEq)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (logShow)
import Merge (mergeWith, mergePoly, merge)
import OneTwoThree (OneTwoThree(..), idOneTwoThree)
import Partial.Unsafe (unsafePartial)
import Sorted (sorted)
import Test.QuickCheck (class Arbitrary, Result(..), arbitrary, quickCheck, (<?>))
import Test.QuickCheck.Gen (arrayOf, elements)
import Tree (Tree, member, insert, toArray, anywhere)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

ints :: Array Int -> Array Int
ints = identity

bools :: Array Boolean -> Array Boolean
bools = identity

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = identity

treeOfInt :: Tree Int -> Tree Int
treeOfInt = identity

oneTwoThreeInt :: OneTwoThree Int -> OneTwoThree Int
oneTwoThreeInt = identity

mergeWithAssocProp xs ys zs f =
    let
      first = map f $ mergeWith f (mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)) (sortBy (compare `on` f) zs)
      second = map f $ mergeWith f (sortBy (compare `on` f) xs) (mergeWith (intToBool f) (sortBy (compare `on` f) ys) (sortBy (compare `on` f) xs))
    in
      eq first second <?> "Result:\n" <> show first <> "\nnot equal to expected:\n" <> show second

main :: Effect Unit
main = do
  -- Tests for module 'Merge'

  quickCheck \xs ys ->
    let
      result = merge (sort xs) (sort ys)
      expected = sort $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ->
    let
      result = merge (sorted xs) []
      expected = sorted xs
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ys ->
    let
      result = merge (sorted xs) (sorted ys)
      expected = sort $ sorted xs <> sorted ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ys ->
    eq (ints $ mergePoly (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)

  quickCheck \xs ys ->
    eq (bools $ mergePoly (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)

  quickCheck \xs ys f ->
    let
      result = map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
      expected = map f $ sortBy (compare `on` f) $ xs <> ys
    in
      eq result expected

  quickCheck \xs ys zs f ->
    let
      first = map f $ mergeWith f (mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)) (sortBy (compare `on` f) zs)
      second = map f $ mergeWith f (sortBy (compare `on` f) xs) (mergeWith (intToBool f) (sortBy (compare `on` f) ys) (sortBy (compare `on` f) zs))
    in
      eq first second

  -- Tests for module 'Tree'

  quickCheck \t a -> member a $ insert a $ treeOfInt t
  quickCheck \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfInt t) || anywhere g t

  -- quickCheck \t a n ->
  --   member a $ foldl (\acc aa -> insert aa acc) (insert a $ treeOfInt t) (range 0 n)
  -- quickCheck \t a b n ->
  --   member a $ applyN (insert b) n (insert a $ treeOfInt t)

  -- Tests for module 'Data.Array'
  quickCheck \a as ->
    let 
      result = ints $ cons a as
      expected = [a] <> as
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \a as ->
    let 
      result = bools $ cons a as
      expected = [a] <> as
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \s ->
    let
      result = lowAlphaIdentity s
      expected = s --LowAlpha (lowAlphaToString s <> "kkt")
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  -- Tests for module 'Byte'
  quickCheck \b -> eq (idByte b) b
  quickCheck \b -> 
    let result = idByte b
    in
      (unwrapByte result) >= 0 && (unwrapByte result) <= 255

  -- Tests for module 'OneTwoThree'
  quickCheck \a ->
    let 
      result = idOneTwoThree (oneTwoThreeInt a)
    in
      eq result a <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show a

newtype LowAlpha = LowAlpha String
derive instance eqLowAlpha :: Eq LowAlpha
-- derive instance newtypeLowAlpha :: Newtype LowAlpha _
derive instance genericLowAlpha :: Generic LowAlpha _
instance showLowAlpha :: Show LowAlpha where
  show = genericShow


instance arbitraryLowAlpha :: Arbitrary LowAlpha where
  arbitrary = map (LowAlpha <<< fromCharArray) $ arrayOf $ elements $ NonEmptyArray $ charRange 'a' 'z'
    where
      charRange :: Char -> Char -> Array Char
      charRange f t = map (\i -> unsafePartial $ fromJust $ fromCharCode i) $ (range `on` toCharCode) f t

lowAlphaIdentity :: LowAlpha -> LowAlpha
lowAlphaIdentity = identity

lowAlphaToString :: LowAlpha -> String
lowAlphaToString (LowAlpha s) = s

allSuccess :: List Result -> Boolean
allSuccess = all isSuccess
  where
    isSuccess Success = true
    isSuccess (Failed _) = false

squashResults :: List Result -> Result
squashResults = fromFirst <<< foldMap maybeFailure
  where
    maybeFailure :: Result -> First Result
    maybeFailure Success = First Nothing
    maybeFailure f = First (Just f)

    fromFirst :: First Result -> Result
    fromFirst (First Nothing) = Success
    fromFirst (First (Just f)) = f