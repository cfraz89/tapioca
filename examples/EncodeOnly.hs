{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | Encoding an encode-only map
module Data.Tapioca.Examples.EncodeOnly where

import Control.Lens
import Text.Pretty.Simple
import Data.Tapioca

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show)

instance CsvMapped BasicRecord where
 csvMap = CsvEncodeMap
    $ "Sample Field 1" |-> to field1 -- Encode-only combinator is preferred
   :| "Computed From Field 2" |-> to field2 . to (++ " plus more")
   :| "Sample Field 3" <-> (#field3 :: Field _ BasicRecord (Maybe Int) _) -- For encode maps, type system needs assistance for using bidrectional / #field notation
   :| "Arbitrary Field" |-> like @_ @_ @String @BasicRecord "Any data"


main :: IO ()
main = pPrint $
  encode HasHeader [ BasicRecord 1 "This is field 2" (Just 3)
                   , BasicRecord 2 "This is field 2 again" (Just 6)
                   ]
