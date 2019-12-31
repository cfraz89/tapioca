{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Tapioca.Internal.Types.CsvMap where

import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Data.Tapioca.Internal.Types.Field
import Data.Tapioca.Internal.Types.ColSep
import Data.Tapioca.Internal.Types.HFoldable
import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.Match
import Data.Tapioca.Internal.Types.GParseRecord
import Data.Tapioca.Internal.Types.Capability

import GHC.Generics
import GHC.TypeLits

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped (t :: [Capability]) r' | r' -> t where
  csvMap :: CsvMap t r'

type CsvDecodeReqs r m =
  ( GenericCsvDecode r m C.NamedRecord
  , GenericCsvDecode r m C.Record
  , Width m
  )

type CsvEncodeReqs r m =
  ( HFoldable m (r -> C.NamedRecord)
  , HFoldable m (r -> C.Record)
  , HFoldable m C.Header -- To List headers
  )

data CsvMap (t :: [Capability]) r where
  CsvMap :: forall m r . (CsvDecodeReqs r (m EncodeDecode r), CsvEncodeReqs r (m EncodeDecode r)) => m EncodeDecode r -> CsvMap EncodeDecode r
  CsvEncode :: forall m r. CsvEncodeReqs r (m Encode r) => m Encode r -> CsvMap Encode r
  CsvDecode :: forall m r. CsvDecodeReqs r (m Decode r) => m Decode r -> CsvMap Decode r

class MakeCsvMap (t :: [Capability]) r m where
  mkCsvMap :: m t r -> CsvMap t r

instance (CsvDecodeReqs r (m EncodeDecode r), CsvEncodeReqs r (m EncodeDecode r)) => MakeCsvMap EncodeDecode r m where
  mkCsvMap = CsvMap

instance CsvEncodeReqs r (m Encode r) => MakeCsvMap Encode r m where
  mkCsvMap = CsvEncode

instance CsvDecodeReqs r (m Decode r) => MakeCsvMap Decode r m where
  mkCsvMap = CsvDecode

-- -- We can't compose codecs since we can't change the type of m (easily)
-- mappingCodec :: (r'' -> r') -> (r' -> r'') -> CsvMap EncodeDecode r' -> CsvMap EncodeDecode r''
-- mappingCodec enc dec (CsvMap (Codec enc' dec') m) = CsvMap (Codec (enc' . enc) (dec . dec')) m

-- -- We can't compose codecs since we can't change the type of m (easily)
-- mappingEncoder :: (r'' -> r') -> CsvMap Encode r' -> CsvMap Encode r''
-- mappingEncoder enc (CsvEncode enc' m) = CsvEncode (enc' . enc) m

infixl 5 .->

class MakeFieldMapping f c t where
  (.->) :: B.ByteString -> Field s f c t r -> FieldMapping s f t r

-- | Create a bidirectional mapping from name to field.
-- | Since t can be encode, decode, or both, leave it open
instance (C.FromField c, C.ToField c) => MakeFieldMapping f c EncodeDecode where
  name .-> field'  = BicodeFM name field'

-- | Create an encode-only mapping from name to field.
instance (C.ToField c, f ~ c) => MakeFieldMapping f c Encode where
  name .-> ef  = EncodeFM name ef

-- | Create a decode-only mapping from name to field.
instance (C.FromField c, f ~ c) => MakeFieldMapping f c Decode where
  name .-> ef  = DecodeFM name ef

-- Maybe this will work with parameterised fieldmappings
class CsvMapped t c => Nestable s f c t r where
  nest :: Field s f c t r -> FieldMapping s f t r

-- | Nest the record at this field into the mapping at this point.
instance forall s r f c. (CsvMapped EncodeDecode c, Generic c) => Nestable s f c EncodeDecode r where
  nest = Nest

instance forall s r f c. (f ~ c, CsvMapped Encode f) => Nestable s f c Encode r where
  nest = NestEncode

instance forall s r f c. (f ~ c, CsvMapped Decode f) => Nestable s f c Decode r where
  nest = NestDecode

class Withable m s f c (t :: [Capability]) r where
  with :: Field s f c t r -> m t c -> FieldMapping s f t r

-- | Nest the record at this field into the mapping at this point.
instance (CsvDecodeReqs c (m EncodeDecode c), CsvEncodeReqs c (m EncodeDecode c)) => Withable m s f c EncodeDecode r where
  with f = With f . mkCsvMap

instance (f ~ c, CsvEncodeReqs c (m Encode c)) => Withable m s f c Encode r where
  with f = WithEncode f . mkCsvMap

instance (f ~ c, CsvDecodeReqs c (m Decode c)) => Withable m s f c Decode r where
  with f = WithDecode f . mkCsvMap

-- | A mapping for a single field in our record.
-- A `CsvMap` is a chain of FieldMappings joined with `:|`
--
-- Can be created with:
--
--   * '.->' to map a single field
--   * 'nest' to nest into this field the mapping of the field's type
--   * 'with' to nest into this field a given mapping
data FieldMapping (s :: Symbol) f (t :: [Capability]) r where
  BicodeFM :: forall s f c t r. (C.FromField c, C.ToField c) => B.ByteString -> Field s f c EncodeDecode r -> FieldMapping s f t r -- Any t allowed for bicoding
  EncodeFM :: forall s f r. C.ToField f => B.ByteString -> Field s f f Encode r -> FieldMapping s f Encode r
  DecodeFM :: forall s f r. C.FromField f => B.ByteString -> Field s f f Decode r -> FieldMapping s f Decode r
  Nest :: forall s f c t r. (CsvMapped EncodeDecode c, Generic c) => Field s f c EncodeDecode r -> FieldMapping s f t r
  NestEncode :: forall s f r. CsvMapped Encode f => Field s f f Encode r -> FieldMapping s f Encode r
  NestDecode :: forall s f r. CsvMapped Decode f => Field s f f Decode r -> FieldMapping s f Decode r
  With :: forall s f c t r. Field s f c EncodeDecode r -> CsvMap EncodeDecode c -> FieldMapping s f t r
  WithEncode :: forall s f r. Field s f f Encode r -> CsvMap Encode f -> FieldMapping s f Encode r
  WithDecode :: forall s f r. Field s f f Decode r -> CsvMap Decode f -> FieldMapping s f Decode r

-- | Match instance
type instance Match (FieldMapping s f t r) s' = EqSymbol s s'

-- | Class for terms which can be reduced to a FieldMapping
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) f (mt :: [Capability]) r | t -> mt where
  selectorMapping :: t -> FieldMapping s f mt r

-- | Reduce instance for a single FieldMapping
instance (r~r', f~f') => Reduce (FieldMapping s f mt r) s f' mt r' where
  selectorMapping = id

-- | Reduce induction
instance (Reduce tt s f mt r, m1 ~ Match (t1 mt r) s, m2 ~ Match (t2 mt r) s, PickMatch ((t1 :| t2) mt r) m1 m2 s, tt ~ Picked ((t1 :| t2) mt r) m1 m2 s) => Reduce ((t1 :| t2) mt r) s f mt r where
 selectorMapping t = selectorMapping (picked @_ @m1 @m2 @s t)

-- | Support for encoding
instance Can 'Encode cs => HFoldVal (FieldMapping s f cs r) (r -> C.Record) where
  hFoldVal fm = case fm of
    BicodeFM _ (Field get (Codec enc _)) -> V.singleton . C.toField . enc . get
    EncodeFM _ (EncodeField e) -> V.singleton . C.toField . e
    DecodeFM _ _ -> error "Decode field not foldable"
    Nest (Field get (Codec enc _)) -> toRecord (csvMap @EncodeDecode) . enc . get
    NestEncode (EncodeField enc) -> toRecord (csvMap @Encode) . enc
    NestDecode _ -> error "Nest decode not foldable"
    With (Field get (Codec enc _)) (cm :: CsvMap EncodeDecode c) -> toRecord cm . enc . get
    WithEncode (EncodeField enc) (cm :: CsvMap Encode c) -> toRecord cm . enc
    WithDecode _ _ -> error "Decode 'with' not foldable"

instance Can 'Encode cs => HFoldVal (FieldMapping s f cs r) (r -> C.NamedRecord) where
  hFoldVal fm = case fm of
    BicodeFM name (Field get (Codec enc _)) -> HM.singleton name . C.toField . enc . get
    EncodeFM name (EncodeField e) -> HM.singleton name . C.toField . e
    DecodeFM _ _ -> error "Decode field not foldable"
    Nest (Field get (Codec enc _)) -> toNamedRecord (csvMap @EncodeDecode) . enc . get
    NestEncode (EncodeField enc) -> toNamedRecord (csvMap @Encode) . enc
    NestDecode _ -> error "Nest decode not foldable"
    With (Field get (Codec enc _)) (cm :: CsvMap EncodeDecode c) -> toNamedRecord cm . enc . get
    WithEncode (EncodeField enc) (cm :: CsvMap Encode c) -> toNamedRecord cm . enc
    WithDecode _ _ -> error "Decode 'with' not foldable"

hFoldOf :: Can 'Encode cs => CsvMap cs r -> C.Header
hFoldOf (CsvMap m) = foldHeader m
hFoldOf (CsvEncode m) = foldHeader m
hFoldOf (CsvDecode _) = error "Cannot fold header for decode map"

-- | Generate a header entry for this mapping
-- instance HFoldVal (FieldMapping s f EncodeDecode r) C.Header where
--   hFoldVal = \case
--     BicodeFM name _ -> pure name
--     Nest (_ :: Field s f c EncodeDecode r) -> hFoldOf (csvMap @_ @c)
--     With (_ :: Field s f c EncodeDecode r) cm -> hFoldOf cm

instance Can 'Encode cs => HFoldVal (FieldMapping s f cs r) C.Header where
  hFoldVal = \case
    BicodeFM name _ -> pure name
    EncodeFM name _ -> pure name
    DecodeFM _ _ -> error "Not providing header for decode mapping"
    Nest (_ :: Field s f c EncodeDecode r) -> hFoldOf (csvMap @_ @c)
    NestEncode (_ :: Field s f f Encode r) -> hFoldOf (csvMap @_ @f)
    NestDecode _ -> error "Not providing header for decode nest mapping"
    With (_ :: Field s f c EncodeDecode r) cm -> hFoldOf cm
    WithEncode (_ :: Field s f f Encode r) cm -> hFoldOf cm
    WithDecode _ _ -> error "Not providing header for decode with mapping"

foldHeader :: HFoldable m C.Header => m -> C.Header
foldHeader = hFoldMap @_ @C.Header id

instance Index (FieldMapping s f t r) s where
  index _ = 0

instance Can 'Decode cs => Width (FieldMapping s f cs r) where
  width = \case
    BicodeFM _ _ -> 1
    DecodeFM _ _ -> 1
    EncodeFM _ _ -> error "No width required for encode fields"
    Nest (_ :: Field s f c EncodeDecode r) -> widthOf (csvMap @_ @c)
    NestEncode _ -> error "No width required for encode fields"
    NestDecode (_ :: Field s f c Decode r) -> widthOf (csvMap @_ @c)
    With _ cm -> widthOf cm
    WithEncode _ _ -> error "No width required for encode fields"
    WithDecode _ cm -> widthOf cm
    where widthOf :: CsvMap cs' c -> Int
          widthOf (CsvMap mapping) = width mapping
          widthOf (CsvDecode mapping) = width mapping
          widthOf (CsvEncode _) = error "No width required for encode mapping"

-- Encoding
-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- Similar to cassava's headerOrder function.
header :: forall r cs. Can 'Encode cs => CsvMap cs r -> C.Header
header (CsvMap mapping) = foldHeader mapping
header (CsvEncode mapping) = foldHeader mapping
header (CsvDecode _) = error "Not providing header for decode mapping"

-- | Encode a single record to a cassava 'Record' by ordering.
toRecord :: forall r cs. Can 'Encode cs => CsvMap cs r -> r -> C.Record
toRecord (CsvMap mapping) record = foldRecord record mapping
toRecord (CsvEncode mapping) record = foldRecord record mapping
toRecord (CsvDecode _) _ = error "Cannot encode a decode mapping"

-- | Encode a single record to a cassava 'NamedRecord'.
toNamedRecord :: forall r cs. Can 'Encode cs => CsvMap cs r -> r -> C.NamedRecord
toNamedRecord (CsvMap mapping) record = foldRecord record mapping
toNamedRecord (CsvEncode mapping) record = foldRecord record mapping
toNamedRecord (CsvDecode _) _ = error "Cannot encode a decode mapping"

-- | Fold over our type-level list, converting our CsvMapped type to a cassava record
-- r - CsvMapped instance type
-- m - Type of mapping list
-- rt - Cassava record type
foldRecord :: forall m r rt. (HFoldable m (r -> rt), Semigroup rt) => r -> m -> rt
foldRecord record = hFoldMap @_ @(r -> rt) ($ record)
