{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Tapioca where

import qualified Data.Binary.Builder as BB
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Csv.Builder as CB
import qualified Data.Vector as V
import Data.HashMap.Strict as HM

data TestItem = TestItem
  { field1 :: Int
  , field2 :: String
  }

infixl 0 :=
data FieldMapping a = forall f. C.ToField f => ByteString := (a -> f)
type CsvMap a = V.Vector (FieldMapping a)

class CsvMapped a where
  csvMap :: CsvMap a 

mkCsvMap :: [FieldMapping a] -> V.Vector (FieldMapping a)
mkCsvMap = V.fromList

newtype CsvRecord a = CsvRecord a

instance forall a. CsvMapped a => C.ToRecord (CsvRecord a) where
  toRecord (CsvRecord a) = (\(_ := getField) -> C.toField $ getField a) <$> csvMap

instance forall a. CsvMapped a => C.ToNamedRecord (CsvRecord a) where
  toNamedRecord (CsvRecord a) = V.foldr' (\(name := getField) -> HM.insert name (C.toField $ getField a)) HM.empty csvMap

instance forall a. CsvMapped a => C.DefaultOrdered (CsvRecord a) where
  headerOrder _ = (\(name := _) -> name) <$> csvMap @a

instance CsvMapped TestItem where
  csvMap = mkCsvMap
    [ "field1" := field1
    , "field2" := field2
    ]

data Header = WithHeader | WithoutHeader

encode :: forall a. CsvMapped a => Header -> [a] -> ByteString
encode withHeader (items :: [a]) = BL.toStrict . BB.toLazyByteString $ case withHeader of
  WithHeader -> CB.encodeHeader (C.headerOrder @(CsvRecord a) undefined) <> recordItems
  WithoutHeader -> recordItems
  where recordItems = mconcat $ CB.encodeRecord . CsvRecord <$> items