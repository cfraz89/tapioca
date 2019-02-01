{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Tapioca.Examples.BasicEncode where

import GHC.Generics
import Data.Tapioca

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

instance CsvMapped BasicRecord where
 csvMap = CsvMap
    $ "Sample Field 1" <-> #field1
   :| "Sample Field 3" <-> #field3
   :| "Sample Field 2" <-> #field2


main :: IO ()
main = do
  let basicCsv = "Sample Field 1,Sample Field 2,Sample Field 3\r\n"
                <> "First,testField,9"

  print $ decode @BasicRecord DecodeNamed basicCsv
