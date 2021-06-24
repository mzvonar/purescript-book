module Byte where

import Prelude
import Data.Show (show)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)


newtype Byte = Byte Int

derive instance eqByte :: Eq Byte

instance arbitraryByte :: Arbitrary Byte where
  arbitrary = map intToByte arbitrary
    where
    intToByte n | n >= 0 = Byte (n `mod` 256)
                | otherwise = intToByte (-n)

instance coarbByte :: Coarbitrary Byte where
  coarbitrary b = coarbitrary b

instance showByte :: Show Byte where
  show (Byte b) = show b

idByte :: Byte -> Byte
idByte = identity

unwrapByte :: Byte -> Int
unwrapByte (Byte b) = b