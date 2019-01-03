{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides support for easier encoding to CSV via the CsvMapped typeclass.

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
  , splice
  , mapEncoder
  , mapDecoder
  , mapCodecs
  ) where

import Data.Tapioca.Internal.Decode
import Data.Tapioca.Internal.Decode.Generic
import Data.Tapioca.Internal.Encode
import Data.Tapioca.Types

import qualified Data.Vector as V
import Type.Reflection

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

splice :: forall r f e d. (GenericCsvDecode d, Typeable f, CsvMapped f, CsvMapped e, CsvMapped d) => FieldMapping r f e d -> SelectorMapping r
splice = Splice