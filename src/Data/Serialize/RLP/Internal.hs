module Data.Serialize.RLP.Internal (
  RLPEncodeable(..),

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
      
  RLPT(..)
      ) where

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
  deriving (Show, Eq) -- just for understanding pourposes and for checking with hspec

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

-- | Internal function for spliting the array in chunks of bytes
rlpSplit :: DBSL.ByteString -> [DBSL.ByteString]
rlpSplit x
  | DBSL.null x        = []
  | DBSL.head x <  192 =
      case () of
        _ | DBSL.head x < 128 -> (DBSL.singleton . DBSL.head $ x) : (rlpSplit $ DBSL.tail x)
          | DBSL.head x < 183 ->
              let size = (fromIntegral $ DBSL.head x) - 128 :: Int in
                let total = size + 1 in
                  (DBSL.take (fromIntegral total) x) : (rlpSplit $ DBSL.drop (fromIntegral total) x)
          | otherwise        ->
                    let sizeSize = (fromIntegral $ DBSL.head x) - 183 :: Int in
                      let size = fromBigEndian . DBSL.take (fromIntegral sizeSize) . DBSL.tail $ x :: Int in
                        let total = sizeSize + size + 1 :: Int in 
                          (DBSL.take (fromIntegral total) x) : (rlpSplit $ DBSL.drop (fromIntegral total) x)
  | DBSL.head x == 192 = (DBSL.singleton $ DBSL.head x) : (rlpSplit $ DBSL.tail x)
  | DBSL.head x <  247 =
      let size = (fromIntegral $ DBSL.head x) - 192 :: Int in
        let total = size + 1 in
          (DBSL.take (fromIntegral total) x) : (rlpSplit $ DBSL.drop (fromIntegral total) x)
  | otherwise          =
      let sizeSize = (fromIntegral $ DBSL.head x) - 247 :: Int in
        let size = fromBigEndian . DBSL.take (fromIntegral sizeSize) . DBSL.tail $ x :: Int in
          let total = sizeSize + size + 1 :: Int in 
            (DBSL.take (fromIntegral total) x) : (rlpSplit $ DBSL.drop (fromIntegral total) x)

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
  rlpEncodeI = runPut . rlpEncodeI'

  -- Use Get to parse the structure
  rlpDecodeI' :: Get a

  -- Mainly run rlpDecodeI'
  rlpDecodeI :: DBSL.ByteString -> Maybe a
  rlpDecodeI x = let r = runGetOrFail rlpDecodeI' x in
                   case r of
                     Left _          -> Nothing
                     Right (_, _, s) -> Just s

--------------------------------------------------------------------------------
-- Instances

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

  rlpDecodeI' = do
    i <- getWord8
    case () of 
      _ | i < 192 -> do         -- ByteArray
            ls <- getRemainingLazyByteString
            return . RLPB . (\(Just x) -> x) . rlpDecodeI $ DBSL.cons i ls
        | i == 192 -> do        -- Empty list
            return $ RLPL []
        | i < 247 -> do         -- Small list
            ls <- getLazyByteString . fromIntegral $ i - 192
            return $ RLPL . map ((\(Just x) -> x) . rlpDecodeI) . rlpSplit $ ls
        | otherwise -> do       -- Big List
            ls <- getLazyByteString . fromIntegral $ i - 247
            let k = fromBigEndian ls
            ls' <- getLazyByteString . fromIntegral $ k
            return $ RLPL . map ((\(Just x) -> x) . rlpDecodeI) . rlpSplit $ ls'

instance RLPEncodeable DBS.ByteString where
  rlpEncodeI' bs
    | (DBS.length bs == 1) && (DBS.head bs < (fromIntegral (128 :: Integer))) = putByteString bs
    | DBS.length bs < 56 = (putWord8 . fromIntegral $ 128 + DBS.length bs)
                           <> (putByteString bs)
    | otherwise = (putWord8 . fromIntegral $ 183 + DBSL.length l)
                  <> (putLazyByteString l)
                  <> (putByteString bs)
        where l = toBigEndian . DBS.length $ bs

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

instance RLPEncodeable Int where
  rlpEncodeI' = rlpEncodeI' . DBSL.toStrict . toBigEndian

  rlpDecodeI' = do
    b <- rlpDecodeI' :: Get DBS.ByteString
    return . fromBigEndian . DBSL.fromStrict $ b
