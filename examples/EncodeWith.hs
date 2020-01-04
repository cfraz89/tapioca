{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Demonstration of basic use of Tapioca to decode a csv
module Data.Tapioca.Examples.EncodeWith where

import GHC.Generics
import Data.Tapioca
import Text.Pretty.Simple

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

data BasicPair = BasicPair { record0 :: BasicRecord, record1 :: BasicRecord }
  deriving (Show, Generic)

instance CsvMapped Encode BasicPair where
  csvMap = mkCsvMap $
    with #record0
       ( "Sample Field 1" .-> #field1
      :| "Sample Field 2" .-> #field2
      :| "Sample Field 3" .-> #field3
       )
    :| with #record1
       ( "Sample Field 4" .-> #field1
      :| "Sample Field 5" .-> #field2
      :| "Sample Field 6" .-> #field3
       )

main :: IO ()
main = pPrint $
  encode HasHeader [BasicPair (BasicRecord 12 "testField" (Just 9)) (BasicRecord 3 "test Again" (Just 7))]
