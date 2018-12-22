{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

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
  , decode
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
import Data.Functor.Contravariant
import Data.List
import Data.Maybe
import Data.Proxy
import qualified Data.Vector as V
import Data.Vector ((!))
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

(?!) :: Maybe a -> String -> Parser a
mb ?! err = maybe (fail err) pure mb

instance (HasField x r f, KnownSymbol x, f ~ d, f ~ e) => IsLabel x (FieldMapping r f e d) where
  fromLabel = FieldMapping (symbolVal' (proxy# :: Proxy# x)) (getField @x) id

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- e - type to encode as
-- g - Type to decode as
data FieldMapping r f e d = FieldMapping
  { selector :: String
  , toEncodable :: r -> e
  , fromDecodable :: d -> f
  }

-- Esentially Functor instance
encodeWith :: C.ToField x => (e -> x) -> FieldMapping r f e d -> FieldMapping r f x d
encodeWith f fm = fm { toEncodable = f . toEncodable fm }

decodeWith :: C.FromField x => (x -> d) -> FieldMapping r f e d -> FieldMapping r f e x
decodeWith f fm = fm { fromDecodable = fromDecodable fm . f }

with :: (C.ToField x, C.FromField y) => (e -> x) -> (y -> d) -> FieldMapping r f e d -> FieldMapping r f x y
with enc dec = encodeWith enc . decodeWith dec

instance Show (FieldMapping r f e d) where
  show fm = "Mapping " <> selector fm <> " (r -> f) (g -> f)"

infixl 0 :=
data NamedFieldMapping r = forall f e d. (C.ToField e, C.FromField d) => B.ByteString := FieldMapping r f e d

instance Show (NamedFieldMapping r) where
  show (name := fm) = show name <> " := " <> show fm

newtype CsvMap r = CsvMap { unCsvMap :: V.Vector (NamedFieldMapping r) }
  deriving Show

-- instance Contravariant CsvMap where
--   contramap f (CsvMap mappings) = CsvMap $ contraMapping <$> mappings
--     where contraMapping (name := FieldMapping{..}) = name := FieldMapping selector (toEncodable . f) fromDecodable

data SelProxy t f a = SelProxy

type GenericCsvDecode r =  (GSelectorList (Rep r), GParseRecord (Rep r), Generic r)

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

class GSelectorList f where
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
  gIndexedParseRecord i selectorOrder record = (\v -> (i+1, M1 $ K1 v)) <$> C.parseField (record ! (selectorOrder ! i))

instance (GIndexedParseRecord a, GIndexedParseRecord b) => GIndexedParseRecord (a :*: b) where
  gIndexedParseRecord i selectorOrder record = do
    (ia, a) <- gIndexedParseRecord i selectorOrder record
    (ib, b) <- gIndexedParseRecord ia selectorOrder record
    pure (ib, a :*: b)

preParser :: forall a. (CsvMapped a, GenericCsvDecode a) => Proxy a -> Header -> Parser (V.Vector Int, C.Csv)
preParser _ header = do
  hdr <- CP.header . fromIntegral . fromEnum $ ','
  selectorMapping <- traverse (positionOf hdr) (V.indexed . unCsvMap $ csvMap @a)
  let selectorOrder = V.update (V.replicate (length selectorMapping) 0) selectorMapping
  records <- CP.csv C.defaultDecodeOptions
  pure (selectorOrder, records)
  where selectors = gSelectorList (Proxy @(Rep a))
        positionOf hdr (i, fieldHeader := fm) = do
          selectorIndex <- elemIndex (selector fm) selectors ?! ("Record type doesn't have selector " <> selector fm)
          headerIndex <- case header of
            WithHeader -> V.elemIndex fieldHeader hdr ?! ("Couldn't find header item " <> show fieldHeader <> " in CSV header")
            WithoutHeader -> pure i
          pure (selectorIndex, headerIndex)

decode :: forall a. (CsvMapped a, GenericCsvDecode a) => Header -> B.ByteString -> Either String (V.Vector a)
decode header bs = do
   (selectorOrder, csv) <- parseOnly (preParser (Proxy @a) header) bs
   C.runParser $ traverse ((to <$>) . gParseRecord selectorOrder) csv

mkCsvMap :: [NamedFieldMapping r] -> CsvMap r
mkCsvMap = CsvMap . V.fromList

newtype CsvRecord a = CsvRecord a

instance CsvMapped r => C.ToRecord (CsvRecord r) where
  toRecord (CsvRecord a) = (\(_ := fm) -> (C.toField . toEncodable fm) a) <$> unCsvMap csvMap

instance CsvMapped r => C.ToNamedRecord (CsvRecord r) where
  toNamedRecord (CsvRecord a) = V.foldr' (\(name := fm) -> HM.insert name (C.toField . toEncodable fm $ a)) HM.empty (unCsvMap csvMap)

instance CsvMapped r => C.DefaultOrdered (CsvRecord r) where
  headerOrder _ = header (Proxy @r)

-- | When encoding, whether or not to write the header row
-- When decoding, whether or not the csv being decoded contains a header row
-- if decoding WithoutHeader, tapioca will map the order of fields in the csv
-- to the order that fields are specified in the csvMap.
data Header = WithHeader | WithoutHeader

-- | Encode a list of items using our mapping
encode :: forall r. CsvMapped r => Header -> [r] -> B.ByteString
encode withHeader items = BL.toStrict . BB.toLazyByteString $ case withHeader of
  WithHeader -> CB.encodeHeader (header (Proxy @r)) <> recordItems
  WithoutHeader -> recordItems
  where recordItems = foldr ((<>) . CB.encodeRecord . CsvRecord) mempty items

-- | We export this since IMO Proxy is nicer than undefined
header :: forall r. CsvMapped r => Proxy r -> V.Vector B.ByteString
header _= (\(name := _) -> name) <$> unCsvMap (csvMap @r)

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
testItems = 
  [ TestRecord 1 "This is field 2" (Just 3)
  , TestRecord 2 "This is field 2 again" (Just 6)
  ]