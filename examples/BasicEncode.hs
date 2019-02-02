{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple mapping of multiple records
module Data.Tapioca.Examples.BasicEncode where

import GHC.Generics
import Text.Pretty.Simple
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
main = pPrint $
  encode HasHeader [ BasicRecord 1 "This is field 2" (Just 3)
                   , BasicRecord 2 "This is field 2 again" (Just 6)
                   ]
