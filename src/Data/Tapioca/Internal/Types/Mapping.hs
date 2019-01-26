{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Data.Tapioca.Internal.Types.Mapping where

import GHC.Exts
import GHC.Generics
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

import Data.Tapioca.Internal.Types.Codec
import Data.Tapioca.Internal.Types.Separator

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Kind
import Data.Type.Bool

type GenericCsvDecode r t i = (GParseRecord (Rep r) r t i, Generic r)

data CsvMap r = forall m.
  ( GenericCsvDecode r m C.NamedRecord
  , GenericCsvDecode r m C.Record
  ) => CsvMap m

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

  (<->) :: forall s f d e. (C.FromField f, C.FromField d) => B.ByteString -> Codec s r f d e -> FieldMapping s r f
  name <-> codec  = Field name codec

--data MapTo s f d e = MapTo (SelectorProxy s) (Codec f d e)

--field :: (f~d, f~e) => SelectorProxy s -> MapTo s f d e
--field s = MapTo s idCodec

data FieldMapping (s :: Symbol) r f where
  Field :: forall s r f d e. C.FromField d => B.ByteString -> Codec s r f d e -> FieldMapping s r f
  Splice :: forall s r f d e. (CsvMapped d, Generic d) =>  Codec s r f d e -> FieldMapping s r f

-- newtype EncodeItem = EncodeItem (B.ByteString -> B.ByteString)
-- instance HFoldable (FieldMapping s r f) EncodeItem where
--   foldVal m = case m of
--     Field _ codec -> decoder codec
--     Splice codec -> hFoldMap 

instance (HasField x r f, r~f, f ~ d, f ~ e, CsvMapped f, Generic f) => IsLabel x (FieldMapping x r f) where
  fromLabel = Splice (Codec id id id)

data SelectorProxy (s :: Symbol) = SelectorProxy

instance (s~s') => IsLabel s (SelectorProxy s') where
  fromLabel = SelectorProxy

-- Type family to determine whether an arbitrary selector matches that of our mapping
type family Match t s :: Bool where
  Match (FieldMapping s _ _) s' = EqSymbol s s'
  Match (t1 :| t2) s = Match t1 s || Match t2 s

-- Finds the correct index within a record of a mapping
type family Index t s :: Nat where
  Index (FieldMapping s _ _) s = 0
  Index (t1 :| t2) s = If (Match t1 s) (Index t1 s) (1 + Index t2 s)

type family OrdBool (o :: Ordering) :: Bool where
  OrdBool 'LT = 'False
  OrdBool 'EQ = 'True
  OrdBool 'GT = 'False

type EqSymbol s s' = OrdBool (CmpSymbol s s')

class PickMatch (t1 :: Type) (t2 :: Type) (b :: Bool) where
  type Picked t1 t2 b
  picked :: t1 :| t2 -> Picked t1 t2 b

instance PickMatch t1 t2 'True where
  type Picked t1 t2 'True = t1
  picked (t1 :| _) = t1

instance PickMatch t1 t2 'False where
  type Picked t1 t2 'False = t2
  picked (_ :| t2) = t2


-- Class for terms which can be reduced to a simpler type
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) r f where
  selectorMapping :: t -> FieldMapping s r f

-- Base case
instance (r~r',f~f') => Reduce (FieldMapping s r f) s r' f' where
  selectorMapping = id

-- Inductive case
instance (Reduce tt s r f, m ~ Match t1 s, PickMatch t1 t2 m, tt ~ Picked t1 t2 m) => Reduce (t1 :| t2) s r f where
 selectorMapping t = selectorMapping (picked @_ @_ @m t)

-- f :: Generic representation
-- r :: record type we are parsing to
-- t :: Our CsvMap unwrapped type
-- i :: Indexing type - Record or NamedRecord
-- namedRecord :: type level map (selector -> data) (
class GParseRecord (f :: Type -> Type) r t i where
  gParseRecord :: Proxy# r -> t -> i -> C.Parser (f p)

instance GParseRecord f r t i => GParseRecord (M1 D x f) r t i where
  gParseRecord p fieldMapping record = M1 <$> gParseRecord p fieldMapping record

instance GParseRecord f r t i => GParseRecord (M1 C x f) r t i where
  gParseRecord p fieldMapping record = M1 <$> gParseRecord p fieldMapping record

instance Reduce t s r f => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping namedRecord = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping fieldMapping of
            Field name fm -> maybe (fail errMsg) decode val
              where errMsg = "No column " <> BC.unpack name <> " in columns: " <> bsVectorString (HM.keys namedRecord)
                    val = HM.lookup name namedRecord
                    decode = (decoder fm <$>) . C.parseField
            Splice (fm :: Codec s r _ d _) -> parseSplice (csvMap @d)
              where parseSplice (CsvMap cm) = decoder fm . to <$> gParseRecord @_ @d proxy# cm namedRecord

instance (Reduce t s r f, idx~Index t s, KnownNat idx) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping fieldMapping of
            Field _ fm -> maybe (fail errMsg) decode val
              where errMsg = "Can't parse item at index " <> show idx <> " in row: " <> bsVectorString (V.toList record)
                    decode = (decoder fm <$>) . C.parseField
                    idx = natVal' @idx proxy#
                    val = record V.!? fromIntegral idx
            Splice (fm :: Codec s r _ d _) -> parseSplice (csvMap @d)
              where parseSplice (CsvMap cm) = decoder fm . to <$> gParseRecord @_ @d proxy# cm record

bsVectorString :: [B.ByteString] -> String
bsVectorString = BC.unpack . BC.intercalate ","
                           
instance (GParseRecord a r t i, GParseRecord b r t i) => GParseRecord (a :*: b) r t i where
  gParseRecord p t mapping = do
    a <- gParseRecord p t mapping
    b <- gParseRecord p t mapping
    pure $ a :*: b
