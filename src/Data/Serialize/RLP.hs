{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      : Data.Serialize.RLP
-- License     : LGPL-3 (see LICENSE)
--
-- Maintainer  : Javier Sagredo <jasataco@gmail.com>
-- Stability   : stable
--
-- An implementation of the Recursive Length Prefix method
-- as described in the Yellow Paper <https://ethereum.github.io/yellowpaper/paper.pdf>.
--
-- To actually use this module, the type that is going to
-- be encoded has to be instance of RLPSerialize defining
-- 'toRLP' and 'fromRLP'.
--------------------------------------------------------------------------------

module Data.Serialize.RLP (
  -- * The RLP Type
  RLPT(..),
  -- ** Subtleties
  -- $subtleties

  -- * Helper Int functions
  toBigEndian,
  toBigEndianS,
  fromBigEndian,
  fromBigEndianS,

  -- * Helper String functions
  toByteString,
  toByteStringS,
  fromByteString,
  fromByteStringS,
  
  -- * The RLPSerialize class
  RLPSerialize(..)
  -- * Example
  -- $example
  
  -- * Errors and special cases
  -- $failexample
  
) where

import Data.Serialize.RLP.Internal

import qualified Data.ByteString              as DBS
import qualified Data.ByteString.Lazy         as DBSL
import qualified Data.ByteString.Char8        as DBSC

--------------------------------------------------------------------------------

-- $subtleties
-- The idea of transforming a custom type into RLPT is to
-- preserve the original structure as far as possible. For example,
-- suppose we have a data structure:
--
-- > data Name   = (String, String)   -- represents the first and last name of a Person
-- > data Person = Person Name Int    -- represents the whole name of a Person and its age
--
-- Then the desired output of the transformation of a Person value to RLPT should be (pseudocode):
--
-- > RLPL [ RLPL [ RLPB, RLPB ], RLPB ]
--
-- This way the structure is clearly preserved. Eventhough this does not have
-- to be true as the transformation to RLPL is defined by the user and a custom
-- process can be implemented, it is advised to follow this guideline for better
-- understanding of the generated code.
--
-- It is important to remark that although it can't be imposed, it doesn't make sense to try
-- to transform to RLP types with more than one constructor that don't difer in structure.
-- The transformation should encode 
-- a way to find out which of the constructors belongs to the data so not only data is being
-- encoded in the result, also information about the structure futher than the actual length
-- prefixes. That's why it only makes sense to transform to RLP types with just one constructor.
-- On the other hand, it's perfectly viable to encode types with more than one constructor if
-- the structure of each of them is different as it can be adjusted via pattern matching
-- strategies.

--------------------------------------------------------------------------------

-- | The 'RLPSerialize' class provides functions for transforming values to RLPT structures.
-- For encoding and decoding values with the RLP protocol, 'toRLP' and 'fromRLP' have to
-- be implemented.
--
-- Instances of RLPSerialize are expected to satisfy the following property:
--
-- > fromRLP . toRLP == id
--
-- In such case, it can be assured with the default definition that:
--
-- > rlpDecode . rlpEncode == id
--
-- RLPSerialize makes use of the Get and Put classes together with a set of
-- custom serializations for encoding and decoding RLPT data.
class RLPSerialize a where
  -- | Transform a value to the 'RLPT' structure that best fits its internal structure
  toRLP :: a -> RLPT
  -- | Transform an 'RLPT' structure back into the value it represents.
  -- Its return type is 'Maybe a' because it can fail
  fromRLP :: RLPT -> Maybe a

  -- | Transform a value to an 'RLPT' structure and then encode it following the
  -- RLP standard.
  rlpEncode :: a -> DBSL.ByteString
  rlpEncode = rlpEncodeI . toRLP

  -- | Transform a ByteString to an 'RLPT' structure following the RLP standard and
  -- then transform it to the original type. It returns 'Left s' when failing on the
  -- decoding of the transforming from RLPT into the required type, and 'Right v' on
  -- success.
  rlpDecode :: DBSL.ByteString -> Either String a
  rlpDecode x = case rlpDecodeI x :: Either String RLPT of
                  Left m  -> Left m
                  Right v -> case fromRLP v of
                    Nothing -> Left "RLPT value couldn't ve transformed into the required type"
                    Just v' -> Right v'

  {-# MINIMAL toRLP, fromRLP #-}

-- RLPT values don't have to be transformed as they already are RLPT
instance RLPSerialize RLPT where
  toRLP = id

  fromRLP = Just . id

-- ByteStrings just have to be encapsulated
-- Also, it only makes sense to disencapsulate from a ByteString
instance RLPSerialize DBS.ByteString where
  toRLP = RLPB

  fromRLP (RLPB b) = Just b
  fromRLP _ = Nothing

-- Ints have to be transformed into its Big-endian form
-- and then they are treated as ByteStrings.
-- The same applies for the inverse transformation. They
-- are treated as ByteStrings and then interpreted as a
-- Big-endian encoded Int.
instance RLPSerialize Int where
  toRLP = toRLP . toBigEndianS

  fromRLP =  maybe Nothing (\s -> if DBSC.head s == '\NUL'
                                  then Nothing
                                  else case fromBigEndianS s of
                                         Left _  -> Nothing
                                         Right v -> Just v ) .
             (fromRLP :: RLPT -> Maybe DBS.ByteString)

-- Serializing lists implies making a list with the serialization
-- of each element
instance {-# OVERLAPPABLE #-} RLPSerialize a => RLPSerialize [a] where
  toRLP = RLPL . map toRLP

  fromRLP (RLPL x) = if any (\a -> case a of
                                Nothing -> True
                                _ -> False) r
                     then Nothing
                     else Just $ map unJust r
    where r = map fromRLP x
  fromRLP _        = Nothing

-- Bools are serialized as [0] or [1] in a ByteArray
-- THIS IS AN ASUMPTION considering Bool equivalent to
-- integers in the range 0..1
instance RLPSerialize Bool where
  toRLP True = RLPB $ toByteStringS "\SOH"
  toRLP False = RLPB $ toByteStringS "\NUL"

  fromRLP x
    | x == toRLP True  = Just True
    | x == toRLP False = Just False
    | otherwise        = Nothing

-- Strings are serialized as ByteStrings
instance {-# OVERLAPPING #-} RLPSerialize String where
  toRLP = RLPB . toByteStringS
  
  fromRLP (RLPB x) = Just $ fromByteStringS x
  fromRLP _        = Nothing

-- Chars are just length-one strings
instance RLPSerialize Char where
  toRLP = RLPB . toByteStringS . (: [])

  fromRLP (RLPB x) = Just $ head $ fromByteStringS x
  fromRLP _        = Nothing

-- Tuples are transformed into Lists
instance (RLPSerialize a, RLPSerialize b) => RLPSerialize (a, b) where
  toRLP (x, y) = RLPL [toRLP x, toRLP y]

  fromRLP (RLPL [x, y]) =
    maybe Nothing
     (\x' -> maybe Nothing
       (\y' -> Just (x', y'))
       (fromRLP y))
     (fromRLP x)
  fromRLP _             = Nothing

instance (RLPSerialize a, RLPSerialize b, RLPSerialize c) => RLPSerialize (a, b, c) where
  toRLP (x, y, z) = RLPL [toRLP x, toRLP y, toRLP z]

  fromRLP (RLPL [x, y, z]) =
    maybe Nothing
     (\x' -> maybe Nothing
       (\y' -> maybe Nothing
         (\z' -> Just (x', y', z'))
         (fromRLP z))
       (fromRLP y))
     (fromRLP x)
  fromRLP _             = Nothing
 
instance (RLPSerialize a, RLPSerialize b, RLPSerialize c, RLPSerialize d) => RLPSerialize (a, b, c, d) where
  toRLP (a1, a2, a3, a4) = RLPL [toRLP a1, toRLP a2, toRLP a3, toRLP a4]

  fromRLP (RLPL [a1, a2, a3, a4]) =
    maybe Nothing
     (\a1' -> maybe Nothing
      (\a2' -> maybe Nothing
       (\a3' -> maybe Nothing
        (\a4' -> Just (a1', a2', a3', a4'))
        (fromRLP a4))
       (fromRLP a3))
      (fromRLP a2))
     (fromRLP a1)
  fromRLP _             = Nothing

instance (RLPSerialize a, RLPSerialize b, RLPSerialize c, RLPSerialize d, RLPSerialize e) => RLPSerialize (a, b, c, d, e) where
  toRLP (a1, a2, a3, a4, a5) = RLPL [toRLP a1, toRLP a2, toRLP a3, toRLP a4, toRLP a5]

  fromRLP (RLPL [a1, a2, a3, a4, a5]) =
    maybe Nothing
     (\a1' -> maybe Nothing
      (\a2' -> maybe Nothing
       (\a3' -> maybe Nothing
        (\a4' -> maybe Nothing
         (\a5' -> Just (a1', a2', a3', a4', a5'))
         (fromRLP a5))
        (fromRLP a4))
       (fromRLP a3))
      (fromRLP a2))
     (fromRLP a1)
  fromRLP _             = Nothing

instance (RLPSerialize a, RLPSerialize b, RLPSerialize c, RLPSerialize d, RLPSerialize e, RLPSerialize f) => RLPSerialize (a, b, c, d, e, f) where
  toRLP (a1, a2, a3, a4, a5, a6) = RLPL [toRLP a1, toRLP a2, toRLP a3, toRLP a4, toRLP a5, toRLP a6]

  fromRLP (RLPL [a1, a2, a3, a4, a5, a6]) =
    maybe Nothing
     (\a1' -> maybe Nothing
      (\a2' -> maybe Nothing
       (\a3' -> maybe Nothing
        (\a4' -> maybe Nothing
         (\a5' -> maybe Nothing
          (\a6' -> Just (a1', a2', a3', a4', a5', a6'))
          (fromRLP a6))
         (fromRLP a5))
        (fromRLP a4))
       (fromRLP a3))
      (fromRLP a2))
     (fromRLP a1)
  fromRLP _             = Nothing

--------------------------------------------------------------------------------

-- $example
-- For a full example, we reproduce the implementation of the Person type as in the
-- subtleties section.
--
-- First of all, we define the type:
--
-- > type Name = (String, String)
-- > data Person = Person {
-- >                    name :: Name,
-- >                    age  :: Int
-- >                } deriving (Show)
--
-- Then we have to make it an instance of RLPSerialize:
--  
-- >instance RLPSerialize Person where
-- >  toRLP p = RLPL [
-- >                RLPL [
-- >                    toRLP . toByteStringS . fst . name $ p,
-- >                    toRLP . toByteStringS . snd . name $ p
-- >                    ],
-- >                toRLP . age $ p]
-- >
-- >  fromRLP (RLPL [ RLPL [ RLPB a, RLPB b ], RLPB c ]) =
-- >    case fromBigEndianS c of
-- >      Right v -> Just $ Person (fromByteStringS a, fromByteStringS b) v
-- >      _       -> Nothing
-- >  fromRLP _ = Nothing
-- 
-- This way, if the decoding gives rise to other structure than the expected, a the
-- resulting value would be 'Nothing'. We can now use our decoder and encoder
-- with our custom type:
--
-- > p = Person ("John", "Snow") 33
-- > e = rlpEncode p
-- > -- "\204\202\132John\132Snow!" ~ [204,202,132,74,111,104,110,132,83,110,111,119,33]
-- > rlpDecode e :: Maybe Person
-- > -- Right (Person {name = ("John","Snow"), age = 33})
--

--------------------------------------------------------------------------------

-- $failexample
-- In case we run into an error situation, depending whether the RLPT structure is not well
-- formed or the generated structure couldn't be transformed into the expected type, an error
-- is returned in the form of a 'Left' value.
--
-- Just to see this as an example, if we chop the resulting ByteString, there's no way to
-- generate a correct RLPT structure so an error is thrown:
--
-- > rlpDecode $ DBSL.take 6 $ rlpEncode $ RLPL [ RLPB $ toByteStringS "John", RLPB $ toByteStringS "Snow" ] :: Either String RLPT
-- > -- Left "not enough bytes"
--
-- On the other hand, if we try to transform an incorrect value from the decoded RLPT we
-- generate a new error:
--
-- > rlpDecode $ rlpEncode $ RLPB $ toByteStringS "\STX" :: Either String Bool
-- > -- Left "RLPT value couldn't ve transformed into the required type"
--
-- If a ByteString is the result of the concatenation of more than one serialized RLPT structure,
-- only the first one would be decoded. This isn't quite specified in the Yellow Paper although it is possible that an error should be thrown when finding trailing bytes:
--
-- > a = rlpEncode $ RLPL [ RLPB $ toByteStringS "John", RLPB $ toByteStringS "Snow" ]
-- > b = DBSL.append a a
-- > -- "\202\132John\132Snow\202\132John\132Snow"
-- > rlpDecode b :: Either String RLPT
-- > -- Right (RLPL [RLPB "John",RLPB "Snow"])
