{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Encoding an encode-only map
module Data.Tapioca.Examples.EncodeOnly where

import Text.Pretty.Simple
import Data.Tapioca

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show)

instance CsvMapped 'Encode BasicRecord where
 csvMap = CsvEncode
    $ "Sample Field 1" <-< #field1
   :| "Computed From Field 2" <-< encoder (++ " plus more") #field2
   :| "Sample Field 3" <-> #field3 -- The bidirectional combinator can still be used
   :| "Arbitrary Field" <-< encodeField (const @String "Any data")


main :: IO ()
main = pPrint $
  encode HasHeader [ BasicRecord 1 "This is field 2" (Just 3)
                   , BasicRecord 2 "This is field 2 again" (Just 6)
                   ]
