{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}

-- | This module provides support for easier encoding to CSV via the CsvMapped typeclass.

-- * Example
-- $example
module Data.Tapioca
  ( FieldMapping(..)
  , CsvMap(..)
  , CsvMapped(..)
  , mkCsvMap
  , CsvRecord(..)
  , Header(..)
  , encode
  , decode
  , mapEncoder
  , mapDecoder
  , mapCodecs
  ) where

import Data.Tapioca.Internal.Decode
import Data.Tapioca.Types

import qualified Data.Binary.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Csv.Builder as CB
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

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

newtype CsvRecord a = CsvRecord a

instance CsvMapped r => C.ToRecord (CsvRecord r) where
  toRecord (CsvRecord a) = V.concatMap toFields (unCsvMap csvMap)
    where toFields (_ := fm) = pure . C.toField . encoder fm $ a
          toFields (Splice fm) = C.toRecord . CsvRecord $ encoder fm a

instance CsvMapped r => C.ToNamedRecord (CsvRecord r) where
  toNamedRecord (CsvRecord a) = V.foldr' (\(name := fm) -> HM.insert name (C.toField . encoder fm $ a)) HM.empty (unCsvMap csvMap)

instance CsvMapped r => C.DefaultOrdered (CsvRecord r) where
  headerOrder _ = header @r


-- | Encode a list of items using our mapping
encode :: forall r. CsvMapped r => Header -> [r] -> B.ByteString
encode withHeader items = BL.toStrict . BB.toLazyByteString $ case withHeader of
  WithHeader -> CB.encodeHeader (header @r) <> recordItems
  WithoutHeader -> recordItems
  where recordItems = foldr ((<>) . CB.encodeRecord . CsvRecord) mempty items

header :: forall r. CsvMapped r => V.Vector B.ByteString
header = V.concatMap names $ unCsvMap (csvMap @r)
  where names (name := _) = pure name
        names (Splice (_ :: FieldMapping r f e d)) = header @f
