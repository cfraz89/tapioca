{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

-- | Demonstration of providing a custom codec to a field
module Data.Tapioca.Examples.CodecField where

import GHC.Generics
import Control.Invertible.Monoidal

import Data.Tapioca

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

instance CsvMapped BasicRecord where
 csvMap = CsvMap
    $ "Sample Field 1" <-> codec (asOrdinal :<->: fromOrdinal) #field1
   :| "Sample Field 3" <-> #field3
   :| "Sample Field 2" <-> #field2

asOrdinal :: Int -> String
asOrdinal = \case
  0 -> "Zeroth?"
  1 -> "First"
  2 -> "Second"
  3 -> "Third"
  x -> show x

fromOrdinal :: String -> Int
fromOrdinal = \case
  "Zeroth?" -> 0
  "First"   -> 1
  "Second"  -> 2
  "Third"   -> 3
  x         -> read x

main :: IO ()
main = do
  let basicCsv = "Sample Field 1,Sample Field 2,Sample Field 3\r\n"
                <> "First,testField,9"

  print $ decode @BasicRecord DecodeNamed basicCsv
