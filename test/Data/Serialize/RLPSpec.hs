module Data.Serialize.RLPSpec (main, spec) where

import Test.Hspec

import Data.Serialize.RLP

import Data.ByteString      as DBS
import Data.ByteString.Lazy as DBSL

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "when encoding _empty_ values" $ do
    it "should encode the null string" $ do
      (DBSL.unpack . rlpEncode . toByteStringS $ "") `shouldBe` [ 0x80 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . toByteStringS $ "" :: Maybe DBS.ByteString) `shouldBe` (Just $ toByteStringS "")

    it "should encode the empty list" $ do
      (DBSL.unpack . rlpEncode . RLPL $ []) `shouldBe` [ 0xc0 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . RLPL $ [] :: Maybe RLPT) `shouldBe` (Just $ RLPL []) 

    it "should encode the integer Zero" $ do
      (DBSL.unpack . rlpEncode . toRLP $ (0 :: Int)) `shouldBe` [ 0x80 ]

    it "should decode it back" $ do
      (maybe Nothing fromRLP $ rlpDecode . rlpEncode . toRLP $ (0 :: Int) :: Maybe Int) `shouldBe` (Just 0)

  context "when encoding ByteArrays" $ do
    it "should encode an array of length 1 and small value" $ do
      (DBSL.unpack . rlpEncode . toByteStringS $ "\NUL") `shouldBe` [ 0x00 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . toByteStringS $ "\NUL" :: Maybe DBS.ByteString) `shouldBe` (Just $ toByteStringS "\NUL")

    it "should encode an array of length 1 and big value" $ do
      (DBSL.unpack . rlpEncode . toByteStringS $ "\150") `shouldBe` [ 0x81, 0x96 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . toByteStringS $ "\150" :: Maybe DBS.ByteString) `shouldBe` (Just $ toByteStringS "\150")

    it "should encode an array of length bigger than one and smaller than 56" $ do
      (DBSL.unpack . rlpEncode . toByteStringS $ "\SOH\SOH") `shouldBe` [ 0x82, 0x01, 0x01 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . toByteStringS $ "\SOH\SOH" :: Maybe DBS.ByteString) `shouldBe` (Just $ toByteStringS "\SOH\SOH")

    it "should encode an array of length bigger than 56 with _small_ length" $ do
      (DBSL.unpack . rlpEncode . toByteStringS . Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 1)
        `shouldBe` ([ 0xb8, 0x3c ] ++ (Prelude.take 60 . Prelude.repeat $ 0x01))

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . toByteStringS . Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 1 :: Maybe DBS.ByteString)
        `shouldBe` (Just $ toByteStringS . Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 0x01)

    it "should encode an array of length bigger than 56 with _big_ length" $ do
      (DBSL.unpack . rlpEncode . toByteStringS . Prelude.map toEnum . Prelude.take 43520 . Prelude.repeat $ 1)
        `shouldBe` ([ 0xb9, 0xaa, 0x00 ] ++ (Prelude.take 43520 . Prelude.repeat $ 0x01))

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . toByteStringS . Prelude.map toEnum . Prelude.take 43520 . Prelude.repeat $ 1 :: Maybe DBS.ByteString)
        `shouldBe` (Just $ toByteStringS . Prelude.map toEnum . Prelude.take 43520 . Prelude.repeat $ 0x01)

  context "when encoding RLP structures" $ do
    it "should encode RLP structures with less than 56 bytes and one element" $ do
      (DBSL.unpack . rlpEncode $ RLPL [ RLPB . toByteStringS $ "\NUL" ]) `shouldBe` [ 0xc1, 0x00 ]
    
    it "should decode it back" $ do
      (rlpDecode . rlpEncode $ RLPL [ RLPB . toByteStringS $ "\NUL" ] :: Maybe RLPT)
        `shouldBe` (Just $ RLPL [ RLPB . toByteStringS $ "\NUL" ])

    it "should encode RLP structures with less than 56 bytes and more than one element" $ do
      (DBSL.unpack . rlpEncode $ RLPL [ RLPB . toByteStringS $ "\NUL" , RLPL [ RLPB . toByteStringS $ "\NUL" ] ])
        `shouldBe` [ 0xc3, 0x00, 0xc1, 0x00 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode $ RLPL [ RLPB . toByteStringS $ "\NUL" , RLPL [ RLPB . toByteStringS $ "\NUL" ] ] :: Maybe RLPT)
        `shouldBe` (Just $ RLPL [ RLPB . toByteStringS $ "\NUL" , RLPL [ RLPB . toByteStringS $ "\NUL" ] ])

    it "should encode RLP structures with more than 56 bytes and one element" $ do
      (DBSL.unpack . rlpEncode $ RLPL [ RLPB . toByteStringS . Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 1 ])
        `shouldBe` ([ 0xf8, 0x3e, 0xb8, 0x3c ] ++ (Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 0x01))

    it "should decode it back" $ do
      (rlpDecode . rlpEncode $ RLPL [ RLPB . toByteStringS . Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 1 ] :: Maybe RLPT)
        `shouldBe` (Just $ RLPL [ RLPB . toByteStringS . Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 1 ])

    it "should encode RLP structures with more than 56 bytes and more than one element" $ do
      (DBSL.unpack . rlpEncode $ RLPL [ RLPB . toByteStringS $ "\NUL" , RLPL [ RLPB . toByteStringS . Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 1 ] ])
        `shouldBe` ([ 0xf8, 0x41, 0x00,  0xf8, 0x3e, 0xb8, 0x3c ] ++ (Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 0x01))

    it "should decode it back" $ do
      (rlpDecode . rlpEncode $ RLPL [ RLPB . toByteStringS $ "\NUL" , RLPL [ RLPB . toByteStringS . Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 1 ] ] :: Maybe RLPT)
        `shouldBe` (Just $ RLPL [ RLPB . toByteStringS $ "\NUL" , RLPL [ RLPB . toByteStringS . Prelude.map toEnum . Prelude.take 60 . Prelude.repeat $ 1 ] ])


  context "when running Ethereum examples" $ do
    it "should encode dog" $ do
      (DBSL.unpack . rlpEncode . toByteStringS $ "dog") `shouldBe` [ 0x83, 0x64, 0x6f, 0x67 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . toByteStringS $ "dog" :: Maybe DBS.ByteString) `shouldBe` (Just $ toByteStringS "dog")

    it "should encode [ cat, dog ]" $ do
      (DBSL.unpack . rlpEncode . RLPL . Prelude.map (RLPB . toByteStringS) $ [ "cat", "dog" ])
        `shouldBe` [ 0xc8, 0x83, 0x63, 0x61, 0x74, 0x83, 0x64, 0x6f, 0x67 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . RLPL . Prelude.map (RLPB . toByteStringS) $ [ "cat", "dog" ] :: Maybe RLPT)
        `shouldBe` (Just $ RLPL . Prelude.map (RLPB . toByteStringS) $ [ "cat", "dog" ])

    it "should encode the set theoretical representation" $ do
      (DBSL.unpack . rlpEncode . toRLP $ RLPL [ RLPL [] , RLPL [ RLPL [] ] , RLPL [ RLPL [] , RLPL [ RLPL [] ] ] ])
        `shouldBe` [ 0xc7, 0xc0, 0xc1, 0xc0, 0xc3, 0xc0, 0xc1, 0xc0 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . toRLP $ RLPL [ RLPL [] , RLPL [ RLPL [] ] , RLPL [ RLPL [] , RLPL [ RLPL [] ] ] ] :: Maybe RLPT)
        `shouldBe` (Just $ RLPL [ RLPL [] , RLPL [ RLPL [] ] , RLPL [ RLPL [] , RLPL [ RLPL [] ] ] ])

    it "should encode LOREM IPSUM" $ do
        (DBSL.unpack . rlpEncode . toByteStringS $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit")
          `shouldBe` [ 0xb8, 0x38, 0x4c, 0x6f, 0x72, 0x65, 0x6d, 0x20, 0x69, 0x70, 0x73, 0x75, 0x6d, 0x20, 0x64, 0x6f, 0x6c, 0x6f, 0x72, 0x20, 0x73, 0x69, 0x74, 0x20, 0x61, 0x6d, 0x65, 0x74, 0x2c, 0x20, 0x63, 0x6f, 0x6e, 0x73, 0x65, 0x63, 0x74, 0x65, 0x74, 0x75, 0x72, 0x20, 0x61, 0x64, 0x69, 0x70, 0x69, 0x73, 0x69, 0x63, 0x69, 0x6e, 0x67, 0x20, 0x65, 0x6c, 0x69, 0x74 ]

    it "should decode it back" $ do
      (rlpDecode . rlpEncode . toByteStringS $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit" :: Maybe DBS.ByteString)
        `shouldBe` (Just $ toByteStringS $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit")
