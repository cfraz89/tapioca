{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides support for easier encoding to CSV via the CsvMapped typeclass.

-- * Example
-- $example
module Data.Tapioca
  ( FieldMapping(..)
  , CsvMap
  , CsvMapped(..)
  , mkCsvMap
  , CsvRecord(..)
  , Header(..)
  , encode
  ) where

import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Exts
import GHC.Records

import qualified Data.Binary.Builder as BB
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Csv.Builder as CB
import Data.Maybe
import Data.Proxy
import qualified Data.Vector as V
import Data.HashMap.Strict as HM

-- $example
-- > data TestItem = TestItem
-- >  { field1 :: Int
-- >  , field2 :: String
-- >  }

-- > instance CsvMapped TestItem where
-- >  csvMap = mkCsvMap
-- >    [ "field1" := field1
-- >    , "field2" := field2
-- >    ]

(?!) :: Maybe a -> String -> Either String a
mb ?! err = maybe (Left err) Right mb

instance (HasField x r a, KnownSymbol x, C.ToField a) => IsLabel x (Mapping r) where
  fromLabel = Mapping (symbolVal' (proxy# :: Proxy# x)) (getField @x)

infixl 0 :=
data Mapping r = forall f. C.ToField f => Mapping String (r -> f)

instance Show (Mapping r) where
  show (Mapping field get) = "Mapping " <> field

data FieldMapping r = ByteString := Mapping r
  deriving Show
type CsvMap r = V.Vector (FieldMapping r)

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class GDecodeNamedRecord (Rep r) => CsvMapped r where
  csvMap :: CsvMap r

class GDecodeNamedRecord f where
  gDecodeNamedRecord :: HM.HashMap String ByteString -> C.NamedRecord -> Either String (f p)

instance GDecodeNamedRecord f => GDecodeNamedRecord (M1 D t f) where
  gDecodeNamedRecord selHeaderMap nr = M1 <$> gDecodeNamedRecord selHeaderMap nr

instance GDecodeNamedRecord f => GDecodeNamedRecord (M1 C t f) where
  gDecodeNamedRecord selHeaderMap nr = M1 <$> gDecodeNamedRecord selHeaderMap nr

instance (C.FromField a, Selector t) => GDecodeNamedRecord (M1 S t (K1 i a)) where
  gDecodeNamedRecord selHeaderMap nr = do
    let sel = selName (undefined :: x t (K1 i a) p)
    header <- HM.lookup sel selHeaderMap ?! ("Selector " <> sel <> " does not exist in CSV map")
    val <- HM.lookup header nr ?! ("Header " <> show header <> " not found in CSV")
    M1 . K1 <$> (C.runParser . C.parseField) val

instance (GDecodeNamedRecord a, GDecodeNamedRecord b) => GDecodeNamedRecord (a :*: b) where
  gDecodeNamedRecord selHeaderMap nr = do
    a <- gDecodeNamedRecord selHeaderMap nr 
    b <- gDecodeNamedRecord selHeaderMap nr
    pure $ a :*: b


gDecodeField :: C.FromField a => ByteString -> Either String (K1 i a p)
gDecodeField = (K1 <$>) . C.runParser . C.parseField

mkCsvMap :: [FieldMapping r] -> V.Vector (FieldMapping r)
mkCsvMap = V.fromList

newtype CsvRecord a = CsvRecord a

instance CsvMapped r => C.ToRecord (CsvRecord r) where
  toRecord (CsvRecord a) = (\(_ := Mapping _ getField) -> C.toField $ getField a) <$> csvMap

instance CsvMapped r => C.ToNamedRecord (CsvRecord r) where
  toNamedRecord (CsvRecord a) = V.foldr' (\(name := Mapping _ getField) -> HM.insert name (C.toField $ getField a)) HM.empty csvMap

instance CsvMapped r => C.DefaultOrdered (CsvRecord r) where
  headerOrder _ = (\(name := _) -> name) <$> csvMap @r

data Header = WithHeader | WithoutHeader

encode :: forall r. CsvMapped r => Header -> [r] -> ByteString
encode withHeader items = BL.toStrict . BB.toLazyByteString $ case withHeader of
  WithHeader -> CB.encodeHeader (C.headerOrder @(CsvRecord r) undefined) <> recordItems
  WithoutHeader -> recordItems
  where recordItems = mconcat $ CB.encodeRecord . CsvRecord <$> items

-- decode :: forall a. CsvMapped a => ByteString -> Either String (V.Vector a)
-- decode bs = 

-- class (Generic a, CsvMapped a) => CsvDecode a where


data TestItem = TestItem
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving Generic

instance CsvMapped TestItem where
 csvMap = mkCsvMap
   [ "field1" := #field1
   , "field2" := #field2
   , "field3" := #field3
   ]
