{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Demonstration of basic use of Tapioca to decode a csv
module Data.Tapioca.Examples.Newtype where

import GHC.Generics
import Data.Tapioca
import Text.Pretty.Simple

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

newtype RecordWrapper = RecordWrapper { unRecordWrapper :: BasicRecord }
  deriving (Show, Generic)

instance CsvMapped 'Bimap RecordWrapper where
 csvMap = mappingCodec unRecordWrapper RecordWrapper . mkCsvMap
    $ "Sample Field 1" <-> #field1
   :| "Sample Field 3" <-> #field3
   :| "Sample Field 2" <-> #field2


main :: IO ()
main = pPrint $
  encode (csvMap @'Bimap) HasHeader [ RecordWrapper $ BasicRecord 1 "This is field 2" (Just 3)
                   , RecordWrapper $ BasicRecord 2 "This is field 2 again" (Just 6)
                   ]
  -- decode @RecordWrapper DecodeNamed
  --    $ "Sample Field 1,Sample Field 2,Sample Field 3\r\n"
  --   <> "12,testField,9"
