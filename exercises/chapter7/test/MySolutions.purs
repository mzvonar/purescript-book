module Test.MySolutions where

import Data.Maybe
import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)
import Data.Validation.Semigroup (V)

-- Note to reader: Add your solutions to this file

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+)

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 (-)

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 (*)

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 div


addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 (-)

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 (*)

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 div


combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
-- combineMaybe (Just a) = ado
--   x <- a
--   in pure x
combineMaybe (Just a) = Just <$> a


stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S+" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> matches "Street" nonEmptyRegex a.street
          <*> matches "City" nonEmptyRegex a.city
          <*> matches "State" stateRegex a.state


data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance genericTree :: Show a => Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t

derive instance eqTree :: Eq a => Eq (Tree a)

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l v r) = Branch (map f l) (f v) (map f r)

instance foldableTree :: Foldable Tree where
  foldl _ b Leaf = b
  foldl f b (Branch l v r) = foldl f (f (foldl f b l) v) r

  foldr _ b Leaf = b
  foldr f b (Branch l v r) = foldr f (f v (foldr f b r)) l

  
  foldMap _ Leaf = mempty
  foldMap f (Branch l v r) = foldMap f l <> f v <> foldMap f r
    


instance traversableTree :: Traversable Tree where
--traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch l v r) = Branch <$> traverse f l <*> f v <*> traverse f r

--sequence :: forall a m. Applicative m => t (m a) -> m (t a)
  sequence :: forall a m. Applicative m => Tree (m a) -> m (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch l v r) = Branch <$> sequence l <*> v <*> sequence r


traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch l v r) = ado
  vv <- f v
  ll <- traversePreOrder f l
  rr <- traversePreOrder f r
  in Branch ll vv rr

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch l v r) = ado
  ll <- traversePostOrder f l
  rr <- traversePostOrder f r
  vv <- f v
  in Branch ll vv rr


type Person
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

person :: String -> String -> Maybe Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: Person -> V Errors Person
validatePersonOptionalAddress p =
  person <$> nonEmpty "First Name" p.firstName
         <*> nonEmpty "Last Name" p.lastName
         <*> traverse validateAddress p.homeAddress
         <*> validatePhoneNumbers "Phone Numbers" p.phones

--traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
--sequence :: forall a m. Applicative m => t (m a) -> m (t a)


sequenceUsingTraverse :: forall a m t. Applicative m => Traversable t => t (m a) -> m (t a)
sequenceUsingTraverse = traverse identity

traverseUsingSequence :: forall a b m t. Applicative m => Traversable t => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f a = sequence $ map f a