{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

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

import Data.Attoparsec.ByteString
import qualified Data.Binary.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Csv.Parser as CP
import qualified Data.Csv.Builder as CB
import Data.List
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

(?!) :: Maybe a -> String -> Parser a
mb ?! err = maybe (fail err) pure mb

instance (HasField x r a, KnownSymbol x, C.ToField a) => IsLabel x (Mapping r) where
  fromLabel = Mapping (symbolVal' (proxy# :: Proxy# x)) (getField @x)

infixl 0 :=
data Mapping r = forall f. C.ToField f => Mapping String (r -> f)

instance Show (Mapping r) where
  show (Mapping field get) = "Mapping " <> field

data FieldMapping r = B.ByteString := Mapping r
  deriving Show
type CsvMap r = V.Vector (FieldMapping r)

data SelProxy t f a = SelProxy

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class (GSelectorList (Rep r), GParseRecord (Rep r), Generic r) => CsvMapped r where
  csvMap :: CsvMap r

class GSelectorList (f :: * -> *) where
  gSelectorList :: Proxy f -> [String]

instance GSelectorList f => GSelectorList (M1 D t f) where
  gSelectorList _ = gSelectorList (Proxy @f)
  
instance GSelectorList f => GSelectorList (M1 C t f) where
  gSelectorList _ = gSelectorList (Proxy @f)

instance Selector t => GSelectorList (M1 S t f) where
  gSelectorList _ = [selName (SelProxy @t @f)]

instance (GSelectorList a, GSelectorList b) => GSelectorList (a :*: b) where
  gSelectorList _ = gSelectorList (Proxy @a) <> gSelectorList (Proxy @b)

class GParseRecord f where
  gParseRecord :: V.Vector Int -> C.Record -> C.Parser (f p)

class GIndexedParseRecord f where
  gIndexedParseRecord :: Int -> V.Vector Int -> C.Record -> C.Parser (Int, f p)

instance GParseRecord f => GParseRecord (M1 D t f) where
  gParseRecord selectorOrder record = M1 <$> gParseRecord selectorOrder record

instance GIndexedParseRecord f => GParseRecord (M1 C t f) where
  gParseRecord selectorOrder record = M1 . snd <$> gIndexedParseRecord 0 selectorOrder record

-- Lifted this indexing technique from cassava :D
instance C.FromField a => GIndexedParseRecord (M1 S t (K1 i a)) where
  gIndexedParseRecord i selectorOrder record = (\v -> (i+1, M1 (K1 v))) <$> C.parseField (record V.! (selectorOrder V.! i))

instance (GIndexedParseRecord a, GIndexedParseRecord b) => GIndexedParseRecord (a :*: b) where
  gIndexedParseRecord i selectorOrder record = do
    (ia, a) <- gIndexedParseRecord i selectorOrder record 
    (ib, b) <- gIndexedParseRecord ia selectorOrder record
    pure (ib, a :*: b)


-- gDecodeField :: C.FromField a => B.ByteString -> Either String (K1 i a p)
-- gDecodeField = (K1 <$>) . C.runParser . C.parseField

--TODO ordering when no header
preParser :: forall a. CsvMapped a => Proxy a -> Header -> Parser (V.Vector Int, C.Csv)
preParser _ header = do
  hdr <- CP.header . fromIntegral . fromEnum $ ','
  selectorMapping <- traverse (positionOf hdr) (V.indexed $ csvMap @a)
  let selectorOrder = V.update (V.replicate (length selectorMapping) 0) selectorMapping
  records <- CP.csv C.defaultDecodeOptions
  pure (selectorOrder, records)
  where selectors = gSelectorList (Proxy @(Rep a))
        positionOf hdr (i, (fieldHeader := (Mapping sel _))) = do
          selectorIndex <- elemIndex sel selectors ?! ("Record type doesn't have selector " <> sel)
          headerIndex <- case header of
            WithHeader -> V.elemIndex fieldHeader hdr ?! ("Couldn't find header item " <> show fieldHeader <> " in CSV header")
            WithoutHeader -> pure i
          pure (selectorIndex, headerIndex)

decode :: forall a. CsvMapped a => Header -> B.ByteString -> Either String (V.Vector a)
decode header bs = do
   (selectorOrder, csv) <- parseOnly (preParser (Proxy @a) header) bs
   C.runParser $ traverse ((to <$>) . gParseRecord selectorOrder) csv

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

encode :: forall r. CsvMapped r => Header -> [r] -> B.ByteString
encode withHeader items = BL.toStrict . BB.toLazyByteString $ case withHeader of
  WithHeader -> CB.encodeHeader (C.headerOrder @(CsvRecord r) undefined) <> recordItems
  WithoutHeader -> recordItems
  where recordItems = mconcat $ CB.encodeRecord . CsvRecord <$> items


data TestRecord = TestRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

instance CsvMapped TestRecord where
 csvMap = mkCsvMap
   [ "Sample Field 1" := #field1
   , "Sample Field 3" := #field3
   , "Sample Field 2" := #field2
   ]

testCsv :: B.ByteString
testCsv = "Sample Field 1,Sample Field 2,Sample Field 3\9,testField,9"

testCsvNoHeader :: B.ByteString
testCsvNoHeader = "1,8,testField2\n42,10,sample data"

testItems :: [TestRecord]
testItems = [TestRecord 1 "This is field 2" (Just 3)]