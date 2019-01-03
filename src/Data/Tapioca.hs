{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  , CsvMap
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

import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Records

import Control.Error.Util
import Data.Attoparsec.ByteString
import qualified Data.Binary.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Csv.Parser as CP
import qualified Data.Csv.Builder as CB
import Data.List
import Data.Proxy
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.HashMap.Strict as HM
import Type.Reflection
import Data.Type.Equality

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

instance (HasField x r f, KnownSymbol x, f ~ d, f ~ e) => IsLabel x (FieldMapping r f e d) where
  fromLabel = FieldMapping (symbolVal @x Proxy) (getField @x) id

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- e - type to encode as
-- g - Type to decode as
data FieldMapping r f e d = FieldMapping
  { selector :: String
  , encoder :: r -> e
  , decoder :: d -> f
  }

-- Esentially Functor instance
mapEncoder :: (e -> x) -> FieldMapping r f e d -> FieldMapping r f x d
mapEncoder f fm = fm { encoder = f . encoder fm }

mapDecoder :: (x -> d) -> FieldMapping r f e d -> FieldMapping r f e x
mapDecoder f fm = fm { decoder = decoder fm . f }

mapCodecs :: (e -> x) -> (y -> d) -> FieldMapping r f e d -> FieldMapping r f x y
mapCodecs enc dec = mapEncoder enc . mapDecoder dec

instance Show (FieldMapping r f e d) where
  show fm = "Mapping " <> selector fm <> " (r -> f) (g -> f)"

infixl 0 :=
data SelectorMapping r = forall f e d. (C.ToField e, C.FromField d, Typeable f) => B.ByteString := FieldMapping r f e d
                       | forall f e d. (GenericCsvDecode d, Typeable f, CsvMapped f, CsvMapped e, CsvMapped d) => Splice (FieldMapping r f e d)

instance Show (SelectorMapping r) where
  show (name := fm) = show name <> " := " <> show fm
  show (Splice fm) = "Splice " <> show fm

newtype CsvMap r = CsvMap { unCsvMap :: V.Vector (SelectorMapping r) }
  deriving (Show, Semigroup, Monoid)

data SelProxy t f a = SelProxy

type GenericCsvDecode r = (GSelectorList (Rep r), GParseRecord (Rep r), Generic r)

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

class GSelectorList f where
  gSelectorList :: [String]

instance GSelectorList f => GSelectorList (M1 D t f) where
  gSelectorList = gSelectorList @f

instance GSelectorList f => GSelectorList (M1 C t f) where
  gSelectorList = gSelectorList @f

instance Selector t => GSelectorList (M1 S t f) where
  gSelectorList = [selName (SelProxy @t @f)]

instance (GSelectorList a, GSelectorList b) => GSelectorList (a :*: b) where
  gSelectorList = gSelectorList @a <> gSelectorList @b

class GParseRecord f where
  gParseRecord :: V.Vector SelectorMeta -> C.Record -> C.Parser (f p)

class GParseSelector f where
  gParseSelector :: Int -> V.Vector SelectorMeta -> C.Record -> C.Parser (Int, f p)

instance GParseRecord f => GParseRecord (M1 D t f) where
  gParseRecord selectorMetas record = M1 <$> gParseRecord selectorMetas record

instance GParseSelector f => GParseRecord (M1 C t f) where
  gParseRecord selectorMetas record = M1 . snd <$> gParseSelector 0 selectorMetas record

instance Typeable a => GParseSelector (M1 S m (K1 i a)) where
  gParseSelector i selectorMetas record = fmap (M1 . K1) . (succ i,) <$> parseSelector (selectorMetas ! i) record

parseSelector :: forall a. Typeable a => SelectorMeta -> C.Record -> C.Parser a
parseSelector (Field tr pos decodeMapper) record
  | Just Refl <- testEquality tr (typeRep @a) = decodeMapper <$> C.parseField (record ! pos)
parseSelector (Record tr metas decodeMapper) record
  | Just Refl <- testEquality tr (typeRep @a) = decodeMapper . to <$> gParseRecord metas record
parseSelector _ _ = fail "Type mismatch. This shouldn't happen!"

instance (GParseSelector a, GParseSelector b) => GParseSelector (a :*: b) where
  gParseSelector i selectorMetas record = do
    (ia, a) <- gParseSelector i selectorMetas record
    (ib, b) <- gParseSelector ia selectorMetas record
    pure (ib, a :*: b)

data SelectorMeta = forall f d. (C.FromField d, Typeable f) => Field (TypeRep f) Int (d -> f)
                 | forall r d. (GenericCsvDecode d, Typeable r) => Record (TypeRep r) (V.Vector SelectorMeta) (d -> r)

instance Show SelectorMeta where
  show (Field _ i _) = "Field " <> show i
  show (Record _ sms _) = "Record " <> show sms
type IndexedSelectorMeta = (Int, SelectorMeta)

-- Parse the required data from the csv file
csvParser :: forall a. (CsvMapped a, GenericCsvDecode a) => Header -> Parser (Maybe (V.Vector B.ByteString), C.Csv)
csvParser useHeader = do
  hdr <- case useHeader of
    WithHeader -> Just <$> (CP.header . fromIntegral . fromEnum) ','
    WithoutHeader -> pure Nothing
  records <- CP.csv C.defaultDecodeOptions
  pure (hdr, records)

-- Used for decoding
pSelectorMetas :: forall a. (CsvMapped a, GenericCsvDecode a) => Maybe (V.Vector B.ByteString) -> Either String (V.Vector SelectorMeta)
pSelectorMetas hdr = do
  selectorMetasUnordered <- traverse (positionOf hdr) (V.indexed . unCsvMap $ csvMap @a)
  pure $ V.update (snd <$> selectorMetasUnordered) selectorMetasUnordered

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

-- For decoding. With selector mapping determine correct position within csv/header list, and attach metadata necessary to construct from generic
positionOf :: forall r. GenericCsvDecode r => Maybe (V.Vector B.ByteString) -> (Int, SelectorMapping r) -> Either String IndexedSelectorMeta
positionOf mbHdr (i, selectorMapping) = do
  let selectors = gSelectorList @(Rep r)
  case selectorMapping of
    fieldHeader := (fm :: FieldMapping r f e d) -> do
      selectorIndex <- elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      headerIndex <- case mbHdr of
        Just hdr -> V.elemIndex fieldHeader hdr ?! "Couldn't find header item " <> show fieldHeader <> " in CSV header"
        Nothing -> pure i
      pure (selectorIndex, Field @f @d typeRep headerIndex (decoder fm))
    Splice (fm :: FieldMapping r f e d) -> do
      selectorIndex <- elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      selectorMetas <- pSelectorMetas @d mbHdr
      pure (selectorIndex, Record @f @d typeRep selectorMetas (decoder fm))

decode :: forall a. (CsvMapped a, GenericCsvDecode a) => Header -> B.ByteString -> Either String (V.Vector a)
decode useHeader bs = do
   (mbHdr, csv) <- parseOnly (csvParser @a useHeader) bs
   selectorMetas <- pSelectorMetas @a mbHdr
   C.runParser $ traverse ((to <$>) . gParseRecord selectorMetas) csv

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

-- | When encoding, whether or not to write the header row
-- When decoding, whether or not the csv being decoded contains a header row
-- if decoding WithoutHeader, tapioca will map the order of fields in the csv
-- to the order that fields are specified in the csvMap.
data Header = WithHeader | WithoutHeader

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
