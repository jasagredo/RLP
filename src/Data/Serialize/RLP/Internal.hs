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

  unJust,
      
  RLPT(..)
      ) where

import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString              as DBS
import qualified Data.ByteString.Char8        as DBSC
import qualified Data.ByteString.Lazy         as DBSL
import qualified Data.ByteString.Lazy.Char8   as DBSLC
import qualified Data.List                    as DL


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

fromBigEndian :: DBSL.ByteString -> Either String Int
fromBigEndian bs =  case bs'' of
                      Left (_, _, msg) -> Left ("can't decode from Big-Endian: " ++ msg)
                      Right (_, _, a)  -> Right $ fromIntegral a
  where bs' = case () of
          _ | DBSL.length bs >= 8 -> bs
            | otherwise -> DBSLC.append (DBSLC.pack $ b) bs
                     where b = take (8 - (fromIntegral . DBSL.length $ bs)) (repeat '\NUL')
        bs'' = runGetOrFail getInt64be $ bs'
                           
-- | Strict version of 'fromBigEndian'
fromBigEndianS :: DBS.ByteString -> Either String Int
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
rlpSplit :: DBSL.ByteString -> Either String [DBSL.ByteString]
rlpSplit x
  | DBSL.null x        = Right []
  | DBSL.head x <  192 =
      case () of
        _ | DBSL.head x < 128 ->
              let aux = rlpSplit $ DBSL.tail x in
                  case aux of
                    Left m  -> Left m
                    Right v -> Right $ (DBSL.singleton . DBSL.head $ x) : v
          | DBSL.head x < 183 ->
              let size = (fromIntegral $ DBSL.head x) - 128 :: Int in
                let total = size + 1 in
                  let aux = rlpSplit $ DBSL.drop (fromIntegral total) x in
                    case aux of
                      Left m  -> Left m
                      Right v -> Right $ (DBSL.take (fromIntegral total) x) : v
          | otherwise        ->
              let sizeSize = (fromIntegral $ DBSL.head x) - 183 :: Int in
                let size = fromBigEndian . DBSL.take (fromIntegral sizeSize) . DBSL.tail $ x in
                  case size of
                    Left m  -> Left m
                    Right v ->
                      let total = sizeSize + v + 1 :: Int in
                        let aux = rlpSplit $ DBSL.drop (fromIntegral total) x in
                            case aux of
                              Left m  -> Left m
                              Right v' -> Right $ (DBSL.take (fromIntegral total) x) : v'
  | DBSL.head x == 192 =
    let aux = (rlpSplit $ DBSL.tail x) in
      case aux of
        Left m  -> Left m
        Right v -> Right $ (DBSL.singleton $ DBSL.head x) : v
  | DBSL.head x <  247 =
      let size = (fromIntegral $ DBSL.head x) - 192 :: Int in
        let total = size + 1 in
          let aux = rlpSplit $ DBSL.drop (fromIntegral total) x in
            case aux of
              Left m  -> Left m
              Right v -> Right $ (DBSL.take (fromIntegral total) x) : v
          
  | otherwise          =
      let sizeSize = (fromIntegral $ DBSL.head x) - 247 :: Int in
        let size = fromBigEndian . DBSL.take (fromIntegral sizeSize) . DBSL.tail $ x in
          case size of
            Left m  -> Left m
            Right v -> 
              let total = sizeSize + v + 1 :: Int in
                let aux = rlpSplit $ DBSL.drop (fromIntegral total) x in
                  case aux of
                    Left m  -> Left m
                    Right v' -> Right $ (DBSL.take (fromIntegral total) x) : v'

-- Just for internal porpouses
unJust :: Maybe a -> a
unJust (Just x) = x
unJust _        = undefined

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
  rlpDecodeI :: DBSL.ByteString -> Either String a
  rlpDecodeI x = let r = runGetOrFail rlpDecodeI' x in
                   case r of
                     Left (_, _, m)  -> Left m
                     Right (_, _, s) -> Right s

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
            let r = rlpDecodeI $ DBSL.cons i ls
            case r of
              Left m  -> fail m
              Right v -> return . RLPB $ v
        | i == 192 -> do        -- Empty list
            return $ RLPL []
        | i < 247 -> do         -- Small list
            ls <- getLazyByteString . fromIntegral $ i - 192
            let k = rlpSplit ls
            case k of
              Left m  -> fail m
              Right v -> do
                let k' = map rlpDecodeI v
                let k'' = map (\e -> case e of
                                  Left m  -> m
                                  Right _ -> "") k'
                case all null k'' of
                  True -> return $ RLPL . map (\(Right x) -> x) $ k'
                  _    -> fail (DL.intercalate ", " k'')
        | otherwise -> do       -- Big List
            ls <- getLazyByteString . fromIntegral $ i - 247
            let k = fromBigEndian ls
            case k of
              Left m  -> fail m
              Right v -> do
                ls' <- getLazyByteString . fromIntegral $ v
                let k' = rlpSplit ls'
                case k' of
                  Left m'  -> fail m'
                  Right v' -> do
                    let k'' = map rlpDecodeI v'
                    let k'3 = map (\e -> case e of
                                  Left m  -> m
                                  Right _ -> "") k''
                    case all null k'3 of
                      True -> return $ RLPL . map (\(Right x) -> x) $ k''
                      _    -> fail (DL.intercalate ", " k'3)

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
            let k = fromBigEndian sbe
            case k of
              Left m  -> fail m
              Right v -> do
                ls <- getByteString v
                return ls
        | otherwise -> fail "Decoding a ByteString with head >= 192"

instance RLPEncodeable Int where
  rlpEncodeI' = rlpEncodeI' . toBigEndianS

  rlpDecodeI' = do
    b <- rlpDecodeI' :: Get DBS.ByteString
    case DBSC.head b of
      '\NUL' -> fail "leading zeroes found when decoding an integer"
      _      -> do
        let k = fromBigEndianS b
        case k of
          Left m  -> fail m
          Right v -> return v
