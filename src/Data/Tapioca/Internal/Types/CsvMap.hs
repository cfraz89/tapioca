{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Data.Tapioca.Internal.Types.CsvMap where

import Control.Lens (Getter, view)
import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Kind

import Data.Tapioca.Internal.Types.Field
import Data.Tapioca.Internal.Types.ColSep
import Data.Tapioca.Internal.Types.HFoldable
import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.Match
import Data.Tapioca.Internal.Types.GParseRecord

import GHC.Generics
import GHC.TypeLits

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

type CsvDecode r (m :: Type -> Type) =
  ( GenericCsvDecode r (m r) C.NamedRecord
  , GenericCsvDecode r (m r) C.Record
  , Width m r
  )

type CsvEncode r (m :: Type -> Type) =
  ( HFoldable (m r) (r -> C.NamedRecord)
  , HFoldable (m r) (r -> C.Record)
  , HFoldable (m r) C.Header -- To List headers
  )

data CsvMap r = forall m. (CsvDecode r m, CsvEncode r m) => CsvMap (m r)
              | forall m. CsvEncode r m => CsvEncodeMap (m r)

infixl 3 <->
-- | Create a bidirectional mapping from name to field.
(<->) :: forall s r f c. (C.FromField c, C.ToField c) => B.ByteString -> Field s r f c -> FieldMapping s f r
name <-> field  = BicodeField name field

infixl 3 |->
-- | Create an encode-only mapping from name to field.
(|->) :: forall r f. (C.ToField f) => B.ByteString -> Getter r f -> FieldMapping "" f r
name |-> getter  = EncodeField @f name getter

-- | Nest the record at this field into the mapping at this point.
nest :: forall s r f c. (CsvMapped c, Generic c) => Field s r f c -> FieldMapping s f r
nest = Nest

-- | A mapping for a single field in our record.
-- A `CsvMap` is a chain of FieldMappings joined with `:|`
--
-- Can be created with:
--
--   * '<->' to map a single field
--   * 'nest' to nest the record at this field
data FieldMapping (s :: Symbol) f r where
  BicodeField :: forall s r f c. (C.FromField c, C.ToField c) => B.ByteString -> Field s r f c -> FieldMapping s f r
  EncodeField :: C.ToField f => B.ByteString -> Getter r f -> FieldMapping "" f r
  Nest :: forall s r f c. (CsvMapped c, Generic c) => Field s r f c -> FieldMapping s f r

-- | Match instance
type instance Match (FieldMapping s f) s' = EqSymbol s s'

-- | Class for terms which can be reduced to a FieldMapping
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) r f where
  selectorMapping :: t -> FieldMapping s f r

-- | Reduce instance for a single FieldMapping
instance (r~r', f~f') => Reduce (FieldMapping s f r) s r' f' where
  selectorMapping = id

-- | Reduce induction
instance (Reduce tt s r f, m1 ~ Match t1 s, m2 ~ Match t2 s, PickMatch t1 t2 m1 m2 s r, tt ~ Picked t1 t2 m1 m2 s r) => Reduce ((t1 :| t2) r) s r f where
 selectorMapping t = selectorMapping (picked @_ @_ @m1 @m2 @s t)

-- | Support for encoding
instance HFoldVal (FieldMapping s f r) (r -> C.Record) where
  hFoldVal fm = case fm of
    BicodeField _ Field{..} -> V.singleton . C.toField . view (_field . _codec)
    EncodeField _ getter -> V.singleton . C.toField . view getter
    Nest Field{..} -> toRecord . view (_field . _codec)

instance HFoldVal (FieldMapping s f r) (r -> C.NamedRecord) where
  hFoldVal fm = case fm of
    BicodeField name Field{..} -> HM.singleton name . C.toField . view (_field . _codec)
    EncodeField name getter -> HM.singleton name . C.toField . view getter
    Nest Field{..} -> toNamedRecord . view (_field . _codec)

-- | Generate a header entry for this mapping
instance HFoldVal (FieldMapping s f r) C.Header where
  hFoldVal (BicodeField name _) = pure name
  hFoldVal (EncodeField name _) = pure name
  hFoldVal (Nest (_ :: Field s r f c)) = hFoldOf (csvMap @c)
    where hFoldOf (CsvMap m) = foldHeader m
          hFoldOf (CsvEncodeMap m) = foldHeader m

foldHeader :: HFoldable m C.Header => m -> C.Header
foldHeader = hFoldMap @_ @C.Header id 

instance Index (FieldMapping s f r) s where
  index _ = 0

instance Width (FieldMapping s f) r where
  width (BicodeField _ _) = 1
  width (EncodeField _ _) = 0 -- Not relevant for encoding
  width (Nest (_ :: Field _ _ _ c)) = widthOf (csvMap @c)
    where widthOf (CsvMap mapping) = width mapping
          widthOf (CsvEncodeMap _) = 0 -- Not relevant for encoding

-- Encoding
-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- Similar to cassava's headerOrder function.
header :: forall r. CsvMapped r => C.Header
header = fromCsvMap (csvMap @r)
  where fromCsvMap (CsvMap m) = foldHeader m
        fromCsvMap (CsvEncodeMap m) = foldHeader m

-- | Encode a single record to a cassava 'Record' by ordering.
toRecord :: forall r. CsvMapped r => r -> C.Record
toRecord record = foldCsvMap (csvMap @r)
  where foldCsvMap (CsvMap mapping) = foldRecord record mapping
        foldCsvMap (CsvEncodeMap mapping) = foldRecord record mapping

-- | Encode a single record to a cassava 'NamedRecord'.
toNamedRecord :: forall r. CsvMapped r => r -> C.NamedRecord
toNamedRecord record = foldCsvMap (csvMap @r)
  where foldCsvMap (CsvMap mapping) = foldRecord record mapping
        foldCsvMap (CsvEncodeMap mapping) = foldRecord record mapping

-- | Fold over our type-level list, converting our CsvMapped type to a cassava record
-- r - CsvMapped instance type
-- m - Type of mapping list
-- rt - Cassava record type
foldRecord :: forall m r rt. (HFoldable m (r -> rt), Semigroup rt) => r -> m -> rt
foldRecord record = hFoldMap @_ @(r -> rt) ($ record)