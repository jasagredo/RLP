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
  deriving (Show) -- just for understanding pourposes

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

  rlpEncodeI = runPut . rlpEncodeI'
  
  rlpDecodeI' = do
    i <- getWord8
    case () of 
      _ | i < 192 -> do
            ls <- case () of
                    _ | i < 128   -> do
                          return $ DBSL.singleton i 
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
                           RLPL _ -> return $ RLPL [ls', ls'']
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

instance RLPEncodeable DBS.ByteString where
  rlpEncodeI' bs
    | (DBS.length bs == 1) && (DBS.head bs < (fromIntegral (128 :: Integer))) = putByteString bs
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


instance RLPEncodeable Int where
  rlpEncodeI' = rlpEncodeI' . DBSL.toStrict . toBigEndian

  rlpEncodeI = runPut . rlpEncodeI'

  rlpDecodeI' = do
    b <- rlpDecodeI' :: Get DBS.ByteString
    return . fromBigEndian . DBSL.fromStrict $ b

  rlpDecodeI = runGet rlpDecodeI'
