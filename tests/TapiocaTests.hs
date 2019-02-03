{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import GHC.Generics
import Data.Tapioca
import Data.Either
import Test.Hspec
import qualified Data.Vector as V

main :: IO ()
main = hspec $ do
  describe "Encodes correctly" $ do
    let basicRecord1 = BasicRecord 1 "test" 2.0
    let basicRecord2 = BasicRecord 2 "data" 4.5

    describe "Flat records" $ do
      let basicRecords = [basicRecord1, basicRecord2]

      it "Encodes flat record with header" $ do
        let encoded = encode HasHeader basicRecords
        encoded `shouldBe` "Column 1,Column 2,Column 3\r\n1,test,2.0\r\n2,data,4.5\r\n"

      it "Encodes flat record without header " $ do
        let encoded = encode NoHeader basicRecords
        encoded `shouldBe` "1,test,2.0\r\n2,data,4.5\r\n"

    describe "Nesting records" $ do
      let nestingRecords =
            [ NestingRecord 1 basicRecord1 "foo" 2.5
            , NestingRecord 2 basicRecord2 "bar" 5.5
            ]

      it "Encodes nesting record with header" $ do
        let encoded = encode HasHeader nestingRecords
        encoded `shouldBe` "SomeInfo 1,SomeInfo 2,Column 1,Column 2,Column 3,SomeInfo 3\r\n1,foo,1,test,2.0,2.5\r\n2,bar,2,data,4.5,5.5\r\n"

      it "Encodes nesting record without header" $ do
        let encoded = encode NoHeader nestingRecords
        encoded `shouldBe` "1,foo,1,test,2.0,2.5\r\n2,bar,2,data,4.5,5.5\r\n"

  describe "Decodes correctly" $ do
    describe "Flat Records" $ do
      it "Decodes named record with header" $ do
        let csv = "Column 1,Column 2,Column 3\r\n1,Buzz,4.0\r\n12,Fizz,7.3"
        let decoded = decode @BasicRecord DecodeNamed csv
        decoded `shouldBe` (Right (V.fromList
                                   [ BasicRecord 1 "Buzz" 4.0
                                   , BasicRecord 12 "Fizz" 7.3
                                   ]))

      it "Decodes named record with columns out of order" $ do
        let csv = "Column 1,Column 3,Column 2\r\n1,4.0,Buzz\r\n12,7.3,Fizz"
        let decoded = decode @BasicRecord DecodeNamed csv
        decoded `shouldBe` (Right (V.fromList
                                   [ BasicRecord 1 "Buzz" 4.0
                                   , BasicRecord 12 "Fizz" 7.3
                                   ]))

      it "Decode as named record without header fails" $ do
        let csv = "1,Buzz,4.0\r\n12,Fizz,7.3"
        let decoded = decode @BasicRecord DecodeNamed csv
        decoded `shouldSatisfy` isLeft

      it "Decodes as ordered record with header" $ do
        let csv = "Column 1,Column 2,Column 3\r\n1,Buzz,4.0\r\n12,Fizz,7.3"
        let decoded = decode @BasicRecord (DecodeOrdered HasHeader) csv
        decoded `shouldBe` (Right (V.fromList
                                   [ BasicRecord 1 "Buzz" 4.0
                                   , BasicRecord 12 "Fizz" 7.3
                                   ]))

      it "Decodes as ordered record without header" $ do
        let csv = "1,Buzz,4.0\r\n12,Fizz,7.3"
        let decoded = decode @BasicRecord (DecodeOrdered NoHeader) csv
        decoded `shouldBe` (Right (V.fromList
                                   [ BasicRecord 1 "Buzz" 4.0
                                   , BasicRecord 12 "Fizz" 7.3
                                   ]))

      it "Decodes as ordered record out of order such that types mismatch is error" $ do
        let csv = "1,4.0,Buzz\r\n12,7.3,Fiz"
        let decoded = decode @BasicRecord (DecodeOrdered NoHeader) csv
        decoded `shouldSatisfy` isLeft

    describe "Nested Records" $ do
      it "Decodes nested record named" $ do
        let csv = "SomeInfo 1,SomeInfo 2,Column 1,Column 2,Column 3\r\n1,foo,2,test,5.6,6.4\r\n2,bar,2,data,4.5, 10.1\r\n"
        let decoded = decode @NestingRecord (DecodeOrdered HasHeader) csv
        decoded `shouldBe` (Right (V.fromList
                                   [ NestingRecord 1 (BasicRecord 2 "test" 5.6) "foo" 6.4
                                   , NestingRecord 2 (BasicRecord 2 "data" 4.5) "bar" 10.1
                                   ]))

      it "Decodes nested record ordered without header" $ do
        let csv = "1,foo,2,test,5.6,6.4\r\n2,bar,2,data,4.5, 10.1\r\n"
        let decoded = decode @NestingRecord (DecodeOrdered NoHeader) csv
        decoded `shouldBe` (Right (V.fromList
                                   [ NestingRecord 1 (BasicRecord 2 "test" 5.6) "foo" 6.4
                                   , NestingRecord 2 (BasicRecord 2 "data" 4.5) "bar" 10.1
                                   ]))

      it "Decodes nested record ordered with out of order columns is error" $ do
        let csv = "1,foo,test,2,5.6,6.4\r\n2,bar,data,2,4.5, 10.1\r\n"
        let decoded = decode @NestingRecord (DecodeOrdered NoHeader) csv
        decoded `shouldSatisfy` isLeft


data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Float
  } deriving (Generic, Eq, Show)

instance CsvMapped BasicRecord where
  csvMap = CsvMap
    $ "Column 1" <-> #field1
   :| "Column 2" <-> #field2
   :| "Column 3" <-> #field3

data NestingRecord = NestingRecord
  { nField1 :: Int
  , nBasicData :: BasicRecord
  , nField2 :: String
  , nField3 :: Float
  } deriving (Generic, Eq, Show)

instance CsvMapped NestingRecord where
  csvMap = CsvMap
    $ "SomeInfo 1" <-> #nField1
   :| "SomeInfo 2" <-> #nField2
   :| nest #nBasicData
   :| "SomeInfo 3" <-> #nField3
