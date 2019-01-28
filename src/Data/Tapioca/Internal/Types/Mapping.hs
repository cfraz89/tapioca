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

import Data.Tapioca.Internal.Types.Codec
import Data.Tapioca.Internal.Common (bsVectorString)

import GHC.Exts
import GHC.Generics
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import Data.Kind
import Data.Type.Bool
import qualified Data.Vector as V

type GenericCsvDecode r t i = (GParseRecord (Rep r) r t i, Generic r)

data CsvMap r = forall m.
  ( GenericCsvDecode r m C.NamedRecord
  , GenericCsvDecode r m C.Record
  , HFoldable m (Reader r (V.Vector C.Field)) -- For encode ToRecord
  , HFoldable m C.Header -- To List headers
  ) => CsvMap m

-- Our joining/induction type for records
infixl 1 :|
data a :| b = a :| b

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

  (<->) :: forall s f d e. (C.FromField f, C.FromField d, C.ToField e) => B.ByteString -> Codec s r f d e -> FieldMapping s r f 1
  name <-> codec  = Field name codec

data FieldMapping (s :: Symbol) r f (w :: Nat) where
  Field :: forall s r f d e. (C.FromField d, C.ToField e) => B.ByteString -> Codec s r f d e -> FieldMapping s r f 1
  Splice :: forall s r f d e. (CsvMapped d, Generic d, CsvMapped e, C.ToRecord e, HFoldVal (CsvMap e) C.Header) =>  Codec s r f d e -> FieldMapping s r f (Width d)

-- Tracking of how many columns a type consumes
type family Width t where
  Width (a :| b) = Width a + Width b
  Width (CsvMap m) = Width m
  Width (FieldMapping _ _ _ w) = w

class HFoldVal (t :: Type) x where
  hFoldVal :: t -> x

class HFoldable (t :: Type) x where
  hFoldr :: (x -> b -> b) -> b -> t -> b
  hFoldMap :: Semigroup m => (x -> m) -> t -> m
  hFoldl :: (b -> x -> b) -> b -> t -> b

-- Basic fold instance
instance {-# Overlappable #-} HFoldVal t x => HFoldable t x where
  hFoldl f b x = f b (hFoldVal x)
  hFoldr f b x = f (hFoldVal x) b
  hFoldMap f x = f (hFoldVal x)

-- Induction over :|
instance {-# Overlapping #-} (HFoldVal t x, HFoldable ts x) => HFoldable (t :| ts) x where
  hFoldl f b (x :| xs) = hFoldl f (hFoldl f b x) xs
  hFoldr f b (x :| xs) = hFoldr f (hFoldr f b  x) xs
  hFoldMap f (x :| xs) = hFoldMap f x <> hFoldMap f xs

-- For C.Record instances
instance HFoldVal (FieldMapping s r f w) (Reader r (V.Vector C.Field)) where
  hFoldVal fm = case fm of
    Field _ codec -> asks $ V.singleton . C.toField . encoder codec . getF codec
    Splice codec -> asks $ C.toRecord . encoder codec . getF codec

instance HFoldVal (FieldMapping s r f w) C.Header where
  hFoldVal (Field name _) = pure name
  hFoldVal (Splice (_ :: Codec s r f d e)) = hFoldVal (csvMap @e)

instance (HasField x r f, r~f, f ~ d, f ~ e, CsvMapped f, Generic f, w ~ Width f, C.ToRecord e, HFoldVal (CsvMap e) C.Header) => IsLabel x (FieldMapping x r f w) where
  fromLabel = Splice (Codec id id id)

data SelectorProxy (s :: Symbol) = SelectorProxy

instance (s~s') => IsLabel s (SelectorProxy s') where
  fromLabel = SelectorProxy

-- Type family to determine whether an arbitrary selector matches that of our mapping
type family Match t s :: Bool where
  Match (FieldMapping s _ _ _) s' = EqSymbol s s'
  Match (t1 :| t2) s = Match t1 s || Match t2 s

-- Finds the correct index within a record of a mapping
type family Index (t :: Type) (s :: Symbol) :: Nat where
  Index (FieldMapping _ _ _ _) s  = 0
  Index (t1 :| t2) s = If (Match t1 s) 0 (Width t1)

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
class Reduce t (s :: Symbol) r f (w :: Nat) where
  selectorMapping :: t -> FieldMapping s r f w

-- Base case
instance (r~r', f~f', w~w') => Reduce (FieldMapping s r f w) s r' f' w' where
  selectorMapping = id

-- Inductive case
instance (Reduce tt s r f w, m ~ Match t1 s, PickMatch t1 t2 m, tt ~ Picked t1 t2 m) => Reduce (t1 :| t2) s r f w where
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

instance Reduce t s r f w => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping namedRecord = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @r @f @w fieldMapping of
            Field name fm -> maybe (fail errMsg) decode val
              where errMsg = "No column " <> BC.unpack name <> " in columns: " <> bsVectorString (HM.keys namedRecord)
                    val = HM.lookup name namedRecord
                    decode = (decoder fm <$>) . C.parseField
            Splice (fm :: Codec s r _ d _) -> parseSplice (csvMap @d)
              where parseSplice (CsvMap cm) = decoder fm . to <$> gParseRecord @_ @d proxy# cm namedRecord

instance (Reduce t s r f w, idx~Index t s, KnownNat idx) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @r @f @w fieldMapping of
            Field _ fm -> maybe (fail errMsg) decode val
              where errMsg = "Can't parse item at index " <> show idx <> " in row: " <> bsVectorString (V.toList record)
                    decode = (decoder fm <$>) . C.parseField
                    idx = natVal' @idx proxy#
                    val = record V.!? fromIntegral idx
            Splice (fm :: Codec s r _ d _) -> parseSplice (csvMap @d)
              where parseSplice (CsvMap cm) = decoder fm . to <$> gParseRecord @_ @d proxy# cm record

instance (GParseRecord a r t i, GParseRecord b r t i) => GParseRecord (a :*: b) r t i where
  gParseRecord p t mapping = do
    a <- gParseRecord p t mapping
    b <- gParseRecord p t mapping
    pure $ a :*: b
