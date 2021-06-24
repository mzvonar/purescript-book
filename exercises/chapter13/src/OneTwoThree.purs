module OneTwoThree where

import Prelude

import Data.Array.NonEmpty (fromArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


data OneTwoThree a = One a | Two a a | Three a a a

derive instance eqOneTwoThree :: Eq a => Eq (OneTwoThree a)
derive instance genericOneTwoThree :: Generic (OneTwoThree a) _
instance showOneTwoThree :: Show a => Show (OneTwoThree a) where
  show a = genericShow a

instance arbOneTwoThree :: Arbitrary a => Arbitrary (OneTwoThree a) where
  arbitrary = oneOf $ unsafePartial $ fromJust $ fromArray [One <$> arbitrary, Two <$> arbitrary <*> arbitrary, Three <$> arbitrary <*> arbitrary <*> arbitrary]

idOneTwoThree :: forall a. OneTwoThree a -> OneTwoThree a
idOneTwoThree = identity