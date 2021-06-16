module Test.MySolutions where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson, jsonParser, printJsonDecodeError)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode.Decoders (decodeArray)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3)
import Data.Generic.Rep (class Generic)
import Data.Int (fromNumber, toNumber) as Int
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import React.Basic.DOM (a, s)
import Test.Examples (Complex, Quadratic, Undefined(..), isUndefined)

-- Note to reader: Add your solutions to this file
foreign import volumeFn :: Fn3 Number Number Number Number

foreign import volumeArrow :: Number -> Number -> Number -> Number

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsImpl :: (Complex -> Complex -> Pair Complex) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair

foreign import unsafeUndefinedValue :: forall a. Undefined a -> a

toMaybe :: forall a. Undefined a -> Maybe a
toMaybe a = if isUndefined a then Nothing else Just $ unsafeUndefinedValue a


foreign import valuesOfMapJson :: Json -> Json

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = encodeJson >>> valuesOfMapJson >>> decodeJson

valuesOfMapGeneric :: forall k v. Ord k => Ord v => EncodeJson k => EncodeJson v => DecodeJson v => Map k v -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = encodeJson >>> valuesOfMapJson >>> decodeJson


foreign import quadraticRootsSetJson :: Json -> Json

quadraticRootsSet :: Quadratic -> Either JsonDecodeError (Set Complex)
quadraticRootsSet = encodeJson >>> quadraticRootsSetJson >>> decodeJson

foreign import safeQuadraticRootsJson :: Json -> Json

newtype WrapPair a = WrapPair (Pair a)

p :: WrapPair Int
p = WrapPair (Pair 1 4)

decodePair
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (WrapPair a)
decodePair decoder json = decodeArray Right json >>= f
  where
    f :: Array Json -> Either JsonDecodeError (WrapPair a)
    f = case _ of
        [a, b] -> (\a b -> WrapPair (Pair a b)) <$> decoder a <*> decoder b
        _ -> Left $ TypeMismatch "Pair"

instance decodeJsonDPair :: (DecodeJson a) => DecodeJson (WrapPair a) where
  decodeJson = decodePair decodeJson

quadraticRootsSafeWrap :: Quadratic -> Either JsonDecodeError (WrapPair Complex)
quadraticRootsSafeWrap = encodeJson >>> quadraticRootsSetJson >>> decodeJson

quadraticRootsSafe :: Quadratic -> Either JsonDecodeError (Pair Complex)
quadraticRootsSafe = quadraticRootsSafeWrap >>> map (\(WrapPair p) -> p)

-- jsonErrorToString :: Either JsonDecodeError (Array (Array Int)) -> Either String (Array (Array Int))
jsonErrorToString :: forall a. Either JsonDecodeError a -> Either String a
jsonErrorToString (Left e) = Left $ printJsonDecodeError e
jsonErrorToString (Right a) = Right a


parseAndDecodeArray2D :: String -> Either String (Array (Array Int))
parseAndDecodeArray2D s = do
  json <- jsonParser s
  jsonErrorToString $ decodeJson json


data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

derive instance eqTree :: (Eq a) => Eq (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance showTree :: (Show a) => Show (Tree a) where
  show t = genericShow t

instance encodeJsonTree :: (EncodeJson a) => EncodeJson (Tree a) where
  encodeJson t = genericEncodeJson t

instance decodeJsonTree :: (DecodeJson a) => DecodeJson (Tree a) where
  decodeJson t = genericDecodeJson t


data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

derive instance eqIntOrString :: Eq IntOrString
derive instance genericIntOrString :: Generic IntOrString _
instance showIntOrString :: Show IntOrString where
    show = genericShow

intOrStringToJson :: IntOrString -> Json
intOrStringToJson (IntOrString_Int a) = A.fromNumber $ Int.toNumber a
intOrStringToJson (IntOrString_String a) = A.fromString a

-- intOrStringToInt :: IntOrString -> Json
-- intOrStringToInt (IntOrString_Int a) = A.fromNumber a
-- intOrStringToInt _ = Nothing

-- intOrStringToString :: IntOrString -> Json
-- intOrStringToString (IntOrString_String a) = A.fromString a
-- intOrStringToString _ = Nothing

intToIntOrString :: Json -> Maybe IntOrString
-- intToIntOrString a | A.isNumber a = flatMap (\b -> IntOrString_Int (Int.fromNumber b)) (A.toNumber a)
intToIntOrString a | A.isNumber a = do
  n <- A.toNumber a
  i <- Int.fromNumber n
  pure $ IntOrString_Int i
intToIntOrString _ = Nothing

stringToIntOrString :: Json -> Maybe IntOrString
stringToIntOrString a | A.isString a = map IntOrString_String (A.toString a)
stringToIntOrString _ = Nothing

jsonToIntOrString :: Json -> Maybe IntOrString
jsonToIntOrString a | A.isNumber a = do
  n <- A.toNumber a
  i <- Int.fromNumber n
  pure $ IntOrString_Int i
jsonToIntOrString a | A.isString a = map IntOrString_String (A.toString a)
jsonToIntOrString _ = Nothing

instance encodeJsonIntOrString :: EncodeJson IntOrString where
  encodeJson a = intOrStringToJson a

instance decodeJsonIntOrString :: DecodeJson IntOrString where
  decodeJson a = do 
    let v = jsonToIntOrString a
    case v of
        Just a -> Right a
        Nothing -> Left (TypeMismatch "kokot")