{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.ReifyRecord
  ( RecordType(..)
  , ReifyRecord(..)
  )
where

import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- Record reification
data RecordType = TRecord | TNamedRecord

class ReifyRecord t where
  type RecordIndex t

  fieldIndex :: B.ByteString -> Int -> RecordIndex t
  lookupRecord :: t -> RecordIndex t -> Maybe C.Field
  recordType :: RecordType

instance ReifyRecord C.Record where
  type RecordIndex C.Record = Int

  fieldIndex _ pos = pos
  lookupRecord = (V.!?)
  recordType = TRecord

instance ReifyRecord C.NamedRecord where
  type RecordIndex C.NamedRecord = C.Name
  fieldIndex fieldHeader _ = fieldHeader
  lookupRecord = flip HM.lookup
  recordType = TNamedRecord
