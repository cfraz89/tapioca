{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Demonstrating how nesting records are treated on decoding
module Data.Tapioca.Examples.NestedDecode where

import GHC.Generics
import Text.Pretty.Simple

import Data.Tapioca

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

instance CsvMapped Decode BasicRecord where
 csvMap = mkCsvMap
    $ "Sample Field 1" .-> #field1
   :| "Sample Field 3" .-> #field3
   :| "Sample Field 2" .-> #field2

data NestingRecord = NestingRecord
  { someData :: String
  , nested :: BasicRecord
  , someOtherData :: Int
  }
  deriving (Show, Generic)

instance CsvMapped Decode NestingRecord where
  csvMap = mkCsvMap
     $ nest #nested
    :| "Other" .-> #someOtherData
    :| "Data" .-> #someData

main :: IO ()
main = pPrint $
  decode @NestingRecord (DecodeOrdered WithHeader)
      $ "Sample Field 1,Sample Field 3,Sample Field 2,Other,Data\r\n"
     <> "76,,general data,2,stuff"
