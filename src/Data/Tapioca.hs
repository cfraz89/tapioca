{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , encoder
  , decoder
  , codec
  ) where

import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Exts
import GHC.Records

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Internal.Types as A
import qualified Data.Binary.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Csv.Parser as CP
import qualified Data.Csv.Builder as CB
-- import Data.Functor.Contravariant
import Data.List
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
encoder :: (e -> x) -> FieldMapping r f e d -> FieldMapping r f x d
encoder f fm = fm { toEncodable = f . toEncodable fm }

decoder :: (x -> d) -> FieldMapping r f e d -> FieldMapping r f e x
decoder f fm = fm { fromDecodable = fromDecodable fm . f }

codec :: (e -> x) -> (y -> d) -> FieldMapping r f e d -> FieldMapping r f x y
codec enc dec = encoder enc . decoder dec

instance Show (FieldMapping r f e d) where
  show fm = "Mapping " <> selector fm <> " (r -> f) (g -> f)"

infixl 0 :=
data SelectorMapping r = forall f e d. (C.ToField e, C.FromField d) => B.ByteString := FieldMapping r f e d
                       | forall f e d. (GenericCsvDecode f, CsvMapped f, CsvMapped e, CsvMapped d) => Splice (FieldMapping r f e d)


instance Show (SelectorMapping r) where
  show (name := fm) = show name <> " := " <> show fm
  show (Splice fm) = "Splice " <> show fm

newtype CsvMap r = CsvMap { unCsvMap :: V.Vector (SelectorMapping r) }
  deriving (Show, Semigroup, Monoid)

-- instance Contravariant CsvMap where
--   contramap f (CsvMap mappings) = CsvMap $ contraMapping <$> mappings
--     where contraMapping (name := FieldMapping{..}) = name := FieldMapping selector (toEncodable . f) fromDecodable

data SelProxy t f a = SelProxy

type GenericCsvDecode r = (GSelectorList (Rep r), GParseRecord (Rep r), Generic r)

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
  gParseRecord :: Int -> V.Vector SelectorPos -> C.Record -> C.Parser (Int, f p)

instance GParseRecord f => GParseRecord (M1 D t f) where
  gParseRecord i selectorPoss record = (\(i, r) -> (i, M1 r)) <$> gParseRecord i selectorPoss record

instance GParseRecord f => GParseRecord (M1 C t f) where
  gParseRecord i selectorPoss record = (\(i, r) -> (i, M1 r)) <$> gParseRecord i selectorPoss record

instance ParseSelector a => GParseRecord (M1 S m (K1 i a)) where
  gParseRecord i selectorPoss record = (\(i, r) -> (i, M1 . K1 $ r)) <$> parseSelector i selectorPoss record

class ParseSelector a where
  parseSelector :: Int -> V.Vector SelectorPos -> C.Record -> C.Parser (Int, a)
  default parseSelector GenericCsvDecode a => Int -> V.Vector SelectorPos -> C.Record -> C.Parser (Int, a)
  parseSelector i poss record = do
    (length, rep) <- gParseRecord 0 poss record
    pure (i + length, to rep)

instance C.FromField a => ParseSelector a where
  parseSelector i poss record =  case poss ! i of
    Field pos -> (\v -> (i+1, v)) <$> C.parseField (record ! pos)
    _ -> fail "Need generic"

instance (GParseRecord a, GParseRecord b) => GParseRecord (a :*: b) where
  gParseRecord i selectorOrder record = do
    (ia, a) <- gParseRecord i selectorOrder record
    (ib, b) <- gParseRecord ia selectorOrder record
    pure (ib, a :*: b)

data SelectorPos = Field Int
                 | Record (V.Vector SelectorPos)
type IndexedSelectorPos = (Int, SelectorPos)

preParser :: forall a. (CsvMapped a, GenericCsvDecode a) => Proxy a -> Header -> Parser (V.Vector SelectorPos, C.Csv)
preParser _ useHeader = do
  hdr <- case useHeader of
    WithHeader -> Just <$> (CP.header . fromIntegral . fromEnum) ','
    WithoutHeader -> pure Nothing
  order <- selectorOrder @a Proxy hdr
  records <- CP.csv C.defaultDecodeOptions
  pure (order, records)

selectorOrder :: forall a. (CsvMapped a, GenericCsvDecode a) => Proxy a -> Maybe (V.Vector B.ByteString) -> Parser (V.Vector SelectorPos)
selectorOrder _ hdr = do
  selectorMapping <- traverse (positionOf hdr) (V.indexed . unCsvMap $ csvMap @a)
  pure $ V.update (V.replicate (length selectorMapping) $ Field 0) selectorMapping

positionOf :: forall r. GenericCsvDecode r => Maybe (V.Vector B.ByteString) -> (Int, SelectorMapping r) -> A.Parser B.ByteString IndexedSelectorPos
positionOf mbHdr (i, selectorMapping) = do
  let selectors = gSelectorList (Proxy @(Rep r))
  case selectorMapping of
    fieldHeader := fm -> do
      selectorIndex <- elemIndex (selector fm) selectors ?! ("Record type doesn't have selector " <> selector fm)
      headerIndex <- case mbHdr of
        Just hdr -> V.elemIndex fieldHeader hdr ?! ("Couldn't find header item " <> show fieldHeader <> " in CSV header")
        Nothing -> pure i
      pure (selectorIndex, Field headerIndex)
    Splice (fm :: FieldMapping r f e d) -> do
      selectorIndex <- elemIndex (selector fm) selectors ?! ("Record type doesn't have selector " <> selector fm)
      recordPositions <- selectorOrder @f Proxy mbHdr
      pure (selectorIndex, Record recordPositions)

decode :: forall a. (CsvMapped a, GenericCsvDecode a) => Header -> B.ByteString -> Either String (V.Vector a)
decode hdr bs = do
   (order, csv) <- parseOnly (preParser @a Proxy hdr) bs
   C.runParser $ traverse ((to . snd <$>) . gParseRecord 0 order) csv

mkCsvMap :: [SelectorMapping r] -> CsvMap r
mkCsvMap = CsvMap . V.fromList

newtype CsvRecord a = CsvRecord a

instance CsvMapped r => C.ToRecord (CsvRecord r) where
  toRecord (CsvRecord a) = V.concatMap toFields (unCsvMap csvMap)
    where toFields (_ := fm) = pure . C.toField . toEncodable fm $ a
          toFields (Splice fm) = C.toRecord . CsvRecord $ toEncodable fm a

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

header :: forall r. CsvMapped r => Proxy r -> V.Vector B.ByteString
header _ = V.concatMap names $ unCsvMap (csvMap @r)
  where names (name := _) = pure name
        names (Splice (_ :: FieldMapping r f e d)) = header @f Proxy

data TestRecord = TestRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

data TestRecord2 = TestRecord2
  { testRecord :: TestRecord
  , other :: Int
  }
  deriving (Show, Generic)

-- asOrdinal :: Int -> String
-- asOrdinal 0 = "Zeroth?" 
-- asOrdinal 1 = "First"
-- asOrdinal 2 = "Second"
-- asOrdinal 3 = "Third"
-- asOrdinal x = "Other: " <> show x

instance CsvMapped TestRecord where
 csvMap = mkCsvMap
   [ "Sample Field 1" := #field1
   , "Sample Field 3" := #field3
   , "Sample Field 2" := #field2
   ]

instance CsvMapped TestRecord2 where
  csvMap = mkCsvMap
    [ Splice #testRecord
    , "Other" := #other
    ]

-- testCsv :: B.ByteString
-- testCsv = "Sample Field 1,Sample Field 2,Sample Field 3\n9,testField,9"

testRecord2 :: B.ByteString
testRecord2 = "Sample Field 1,Sample Field 3,Sample Field 2,Other\r\n1,3,This is field 2,4\r\n"

-- testCsvNoHeader :: B.ByteString
-- testCsvNoHeader = "1,8,testField2\n42,10,sample data"

testItems :: [TestRecord]
testItems = 
  [ TestRecord 1 "This is field 2" (Just 3)
  , TestRecord 2 "This is field 2 again" (Just 6)
  ]

testItems2 :: [TestRecord2]
testItems2 = 
  [ TestRecord2 (TestRecord 1 "This is field 2" (Just 3)) 4
  ]