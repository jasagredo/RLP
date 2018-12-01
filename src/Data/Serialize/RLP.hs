-- |
-- Module      : Data.Serialize.RLP
-- License     : GPL-V3
--
-- Maintainer  : jasataco@gmail.com
--
-- An implementation of the Recursive Length Prefix method
-- as described in the Yellow Paper <https://ethereum.github.io/yellowpaper/paper.pdf>.
--
-- To actually use this module, the type that is going to
-- be encoded has to be instance of the RLPSerialize defining
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
  
) where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString              as DBS
import qualified Data.ByteString.Char8        as DBSC
import qualified Data.ByteString.Lazy         as DBSL
import qualified Data.ByteString.Lazy.Char8   as DBSLC

--------------------------------------------------------------------------------
-- | The 'RLPT' type represents the result of transforming the
-- initial data into its byte-array representation, taking in
-- account the structure of the fields.
--
-- Fields that can't be directly transformed into a ByteString (such
-- as a type with several fields) should generate a list with the
-- representations of its fields (using the RLPL constructor).
--
-- RLPT represents the T type defined in the Ethereum Yellowpaper for
-- defining the RLP protocol.
data RLPT = RLPL [RLPT] | RLPB DBS.ByteString
  deriving (Show) -- just for understanding pourposes

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
-- to transform to RLP types with more than one constructor. The transformation should encode
-- a way to find out which of the constructors belongs to the data so not only data is being
-- encoded in the result, also information about the structure futher than the actual length
-- prefixes. That's why it only makes sense to transform to RLP types with just one constructor.
--------------------------------------------------------------------------------

toBigEndian :: Int -> DBSL.ByteString
toBigEndian = DBSLC.dropWhile (== '\NUL') . runPut . putInt64be . fromIntegral

-- | Strict version of 'toBigEndian'
toBigEndianS :: Int -> DBS.ByteString
toBigEndianS = DBSL.toStrict . toBigEndian

fromBigEndian :: DBSL.ByteString -> Int
fromBigEndian bs =  fromIntegral . runGet getInt64be $ bs'
  where bs' = case () of
          _ | DBSL.length bs >= 8 -> bs
            | otherwise -> DBSLC.append (DBSLC.pack $ b) bs
                     where b = take (8 - (fromIntegral . DBSL.length $ bs)) (repeat '\NUL')
                           
-- | Strict version of 'fromBigEndian'
fromBigEndianS :: DBS.ByteString -> Int
fromBigEndianS = fromBigEndian . DBSL.fromStrict

toByteString :: String -> DBSL.ByteString
toByteString = DBSLC.pack

-- | Strict version of 'toByteString'
toByteStringS :: String -> DBS.ByteString
toByteStringS = DBSC.pack

fromByteString :: DBSL.ByteString -> String
fromByteString = DBSLC.unpack

-- | Strict version of 'fromByteString'
fromByteStringS :: DBS.ByteString -> String
fromByteStringS = DBSC.unpack


--------------------------------------------------------------------------------

-- | The 'RLPSerialize' class provides functions for transforming values to RLPT structures.
-- For encoding and decoding values with the RLP protocol, 'toRLP' and 'fromRLP' have to
-- be implemented.
--
-- Instances of RLPSerialize have to satisfy the following property:
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
  -- | Transform an 'RLPT' structure back into the value it represents
  fromRLP :: RLPT -> a

  -- | Transform a value to an 'RLPT' structure and then encode it following the
  -- RLP standard.
  rlpEncode :: a -> DBSL.ByteString
  rlpEncode = rlpEncodeI . toRLP

  -- | Transform a ByteString to an 'RLPT' structure following the RLP standard and
  -- then transform it to the original type.
  rlpDecode :: DBSL.ByteString -> a
  rlpDecode = fromRLP . rlpDecodeI

  {-# MINIMAL toRLP, fromRLP #-}

--------------------------------------------------------------------------------


-- | The 'RLPEncodeable' class groups the RLPT, ByteString and Int types
-- for transforming them into ByteStrings.
--
-- This class defines only the functions for the types explicitly shown on the
-- Yellow Paper. This class intends to be internal and not be used outside the
-- RLPSerialize class.
class RLPEncodeable a where
  -- Use Put to encode the structure
  rlpEncodeI' :: a -> Put

  -- Mainly run rlpEncodeI'
  rlpEncodeI :: a -> DBSL.ByteString

  -- Use Get to parse the structure
  rlpDecodeI' :: Get a

  -- Mainly run rlpDecodeI'
  rlpDecodeI :: DBSL.ByteString -> a


--------------------------------------------------------------------------------

instance RLPEncodeable RLPT where
  rlpEncodeI' (RLPB bs) = rlpEncodeI' bs
  rlpEncodeI' (RLPL t) = case () of
                      _ | DBSL.length dat < 56 -> (putWord8 . fromIntegral $ 192 + DBSL.length dat)
                                                  <> (putLazyByteString dat) 
                        | otherwise -> (putWord8 . fromIntegral $ 247 + DBSL.length l)
                                       <> (putLazyByteString l)
                                       <> (putLazyByteString dat)
                            where l = toBigEndian . fromIntegral . DBSL.length $ dat
    where dat = DBSL.concat . map rlpEncodeI $ t

  rlpEncodeI = runPut . rlpEncodeI'
  
  rlpDecodeI' = do
    i <- getWord8
    case () of 
      _ | i < 192 -> do
            ls <- case () of
                    _ | i < 128   -> do
                          return $ DBSL.singleton i --getLazyByteString . fromIntegral $ 1
                      | i < 183   -> getLazyByteString . fromIntegral $ i - 128
                      | otherwise -> do
                          aux  <- getLazyByteString . fromIntegral $ i - 183
                          aux' <- getLazyByteString . fromIntegral . fromBigEndian $ aux
                          return $ DBSL.append aux $ aux'
            let ls' =  RLPB . rlpDecodeI $ DBSL.cons i ls
            b <- isEmpty :: Get Bool
            case b of
              True -> return ls'
              _    -> do
                         ls'' <- rlpDecodeI' :: Get RLPT
                         case ls'' of
                           RLPB _ -> return $ RLPL [ls', ls'']
                           RLPL t -> return $ RLPL [ls', ls'']
        | i < 247 -> do
            ls <- getLazyByteString . fromIntegral $ i - 192
            let k = rlpDecodeI ls :: RLPT
            b <- isEmpty :: Get Bool
            case b of
              True -> return k
              _ -> do
                ls' <- rlpDecodeI' :: Get RLPT
                case ls' of
                  RLPB _ -> return $ RLPL [k, ls']
                  RLPL t -> return $ RLPL $ k:t
        | otherwise -> do
            ls <- getLazyByteString . fromIntegral $ i - 247
            let k = fromBigEndian ls
            ls' <- getLazyByteString . fromIntegral $ k
            let k' = rlpDecodeI ls' :: RLPT
            b <- isEmpty :: Get Bool
            case b of
              True -> return k'
              _ -> do
                ls'' <- rlpDecodeI' :: Get RLPT
                case ls'' of
                  RLPB _ -> return $ RLPL [k', ls'']
                  RLPL t -> return $ RLPL $ k':t
         
  rlpDecodeI = runGet rlpDecodeI'

-- RLPT values don't have to be transformed as they already are RLPT
instance RLPSerialize RLPT where
  toRLP = id

  fromRLP = id

--------------------------------------------------------------------------------
  
instance RLPEncodeable DBS.ByteString where
  rlpEncodeI' bs
    | (DBS.length bs == 1) && (DBS.head bs < fromIntegral 128) = putByteString bs
    | DBS.length bs < 56 = (putWord8 . fromIntegral $ 128 + DBS.length bs)
                           <> (putByteString bs)
    | otherwise = (putWord8 . fromIntegral $ 183 + DBSL.length l)
                  <> (putLazyByteString l)
                  <> (putByteString bs)
        where l = toBigEndian . DBS.length $ bs

  rlpEncodeI = runPut . rlpEncodeI'

  rlpDecodeI' = do
    i <- getWord8
    case () of
      _ | i < 128 -> return $ DBS.singleton i
        | i < 183 -> do
            ls <- getByteString . fromIntegral $ i - 128
            return ls
        | i < 192 -> do
            sbe <- getLazyByteString . fromIntegral $ i - 183
            ls <- getByteString . fromBigEndian $ sbe
            return ls
        | otherwise -> undefined

  rlpDecodeI = runGet rlpDecodeI'

-- ByteStrings just have to be encapsulated
-- Also, it only makes sense to disencapsulate from a ByteString
instance RLPSerialize DBS.ByteString where
  toRLP = RLPB

  fromRLP (RLPB b) = b
  fromRLP _ = undefined

--------------------------------------------------------------------------------

instance RLPEncodeable Int where
  rlpEncodeI' = rlpEncodeI' . DBSL.toStrict . toBigEndian

  rlpEncodeI = runPut . rlpEncodeI'

  rlpDecodeI' = do
    b <- rlpDecodeI' :: Get DBS.ByteString
    return . fromBigEndian . DBSL.fromStrict $ b

  rlpDecodeI = runGet rlpDecodeI'

-- Ints have to be transformed into its Big-endian form
-- and then they are trated as ByteStrings.
-- The same applies for the inverse transformation. They
-- are treated as ByteStrings and then interpreted as a
-- Big-endian encoded Int.
instance RLPSerialize Int where
  toRLP = toRLP . toBigEndianS

  fromRLP = fromBigEndianS . (fromRLP :: RLPT -> DBS.ByteString)

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
-- > instance RLPSerialize Person where
-- >   toRLP p = RLPL [
-- >                 RLPL [
-- >                     toRLP . toByteStringS . fst . name $ p,
-- >                     toRLP . toByteStringS . snd . name $ p
-- >                     ],
-- >                 toRLP . age $ p]
-- > 
-- >   fromRLP (RLPL [ RLPL [ RLPB a, RLPB b ], RLPB c ]) =
-- >          Person (fromByteStringS a, fromByteStringS b) (fromBigEndianS c :: Int)
-- 
-- This way, if the decoding gives rise to other structure than the expected, a runtime
-- exception will be thrown by the pattern matching. We can now use our decoder and encoder
-- with our custom type:
--
-- > p = Person ("John", "Snow") 33
-- > e = rlpEncode p
-- > -- "\204\202\132John\132Snow!" ~ [204,202,132,74,111,104,110,132,83,110,111,119,33]
-- > rlpDecode e
-- > -- Person {name = ("John","Snow"), age = 33}
--
