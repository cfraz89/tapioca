module Data.Tapioca.Internal.Encode
  ( toRecord
  , toNamedRecord
  ) where

import Data.Tapioca.Internal.Types
  ( CsvMapped(..)
  , FieldMapping(..)
  , Codec(..)
  )

import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- | Tapioca equivalent of cassava's toRecord
toRecord :: CsvMapped r => r -> C.Record
toRecord = undefined
-- toRecord record = foldMap toFields (unCsvMap csvMap)
--     where toFields (_ := fm) = V.singleton . C.toField $ encoder fm record
--           toFields (Splice fm) = toRecord $ encoder fm record

-- | Tapioca equivalent of cassava's toNamedRecord
toNamedRecord :: CsvMapped r => r -> C.NamedRecord
toNamedRecord = undefined
-- toNamedRecord record = foldMap namedRecord (unCsvMap csvMap)
--     where namedRecord (name := fm) = HM.singleton name (C.toField $ encoder fm record)
--           namedRecord (Splice fm) = toNamedRecord $ encoder fm record
