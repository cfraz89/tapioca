{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides support for easier encoding to CSV via the CsvMapped typeclass.
-- | Refer to README.md and examples project for usage

-- * Example
-- $example
module Data.Tapioca
  ( FieldMapping(..)
  , CsvMap(..)
  , CsvMapped(..)
  , CsvRecord(..)
  , Header(..)
  , SelectorMapping (..)
  , encode
  , decode
  , header
  , mkCsvMap
  , mapEncoder
  , mapDecoder
  , mapCodecs
  ) where

import Data.Tapioca.Internal.Decode
import Data.Tapioca.Internal.Encode
import Data.Tapioca.Types

import qualified Data.Vector as V

-- $example
-- > data TestItem = TestItem
-- >  { field1 :: Int
-- >  , field2 :: String
-- >  }

-- > instance CsvMapped TestItem where
-- >  csvMap = mkCsvMap
-- >    [ "field1" := #field1
-- >    , "field2" := #field2
-- >    ]

-- Esentially Functor instance
mapEncoder :: (e -> x) -> FieldMapping r f e d -> FieldMapping r f x d
mapEncoder f fm = fm { encoder = f . encoder fm }

mapDecoder :: (x -> d) -> FieldMapping r f e d -> FieldMapping r f e x
mapDecoder f fm = fm { decoder = decoder fm . f }

mapCodecs :: (e -> x) -> (y -> d) -> FieldMapping r f e d -> FieldMapping r f x y
mapCodecs enc dec = mapEncoder enc . mapDecoder dec

mkCsvMap :: [SelectorMapping r] -> CsvMap r
mkCsvMap = CsvMap . V.fromList