{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-- | A demonstration that Fields are interpreted in
-- the order of the mapping if the header row does not exist
module Data.Tapioca.Examples.BasicDecodeNoHeader where

import GHC.Generics
import Data.Tapioca
import Text.Pretty.Simple

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


main :: IO ()
main = pPrint $
  decode @BasicRecord (DecodeOrdered NoHeader)
      $ "1,8,testField2\r\n"
     <> "42,10,sample data"