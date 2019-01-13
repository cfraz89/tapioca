{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tapioca.Internal.ByCsvMap where

import Data.Tapioca.Internal.Types
import Data.Tapioca.Internal.Common (header)
import Data.Tapioca.Internal.Encode (toRecord, toNamedRecord)
import Data.Tapioca.Internal.Decode (ParseRecord(..), parseRecord)
import Data.Tapioca.Internal.Decode.Generic (GenericCsvDecode)

import qualified Data.Csv as C

-- | A newtype which provides instances for Cassava's To*, From*, and DefaultOrdered typeclasses
newtype ByCsvMap a = ByCsvMap { unByCsvMap :: a }

-- | Provides Cassava instances for our records wrapped in ByCsvMap.
instance CsvMapped r => C.ToRecord (ByCsvMap r) where
  toRecord (ByCsvMap a) = toRecord a

instance CsvMapped r => C.ToNamedRecord (ByCsvMap r) where
  toNamedRecord (ByCsvMap a) = toNamedRecord a

instance CsvMapped r => C.DefaultOrdered (ByCsvMap r) where
  headerOrder _ = header @r

instance (CsvMapped r, GenericCsvDecode r) => C.FromRecord (ByCsvMap r) where
  parseRecord = (ByCsvMap <$>) . parseRecord (Record Nothing)

instance (CsvMapped r, GenericCsvDecode r) => C.FromNamedRecord (ByCsvMap r) where
  parseNamedRecord = (ByCsvMap <$>) . parseRecord NamedRecord
