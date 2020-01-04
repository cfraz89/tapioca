{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Demonstration of basic use of Tapioca to decode a csv
module Data.Tapioca.Examples.DecodeWith where

import GHC.Generics
import Data.Tapioca
import Text.Pretty.Simple

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: String
  }
  deriving (Show, Generic)

data BasicPair = BasicPair { record0 :: BasicRecord, record1 :: BasicRecord }
  deriving (Show, Generic)

instance CsvMapped Decode BasicPair where
  csvMap = mkCsvMap $
    with #record0
       ( "Sample Field 1" .-> #field1
      :| "Sample Field 2" .-> #field2
      :| "Sample Field 3" .-> decoder ((++ " schmeckles") . show . (+ 1) . read @Int) #field3
       )
    :| with #record1
       ( "Sample Field 4" .-> #field1
      :| "Sample Field 5" .-> #field2
      :| "Sample Field 6" .-> #field3
       )

main :: IO ()
main = pPrint $
  decode @BasicPair DecodeNamed
     $ "Sample Field 1,Sample Field 2,Sample Field 3,Sample Field 4,Sample Field 5,Sample Field 6\r\n"
    <> "12,testField,9,3,test Again,7"
