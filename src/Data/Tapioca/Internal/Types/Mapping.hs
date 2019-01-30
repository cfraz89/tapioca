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
import GHC.TypeLits

import Control.Invertible.Monoidal
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import Data.Kind
import Data.Type.Bool
import qualified Data.Vector as V

-- Encoding
-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- | Similar to cassava's headerOrder function
header :: forall r. CsvMapped r => C.Header
header = fromCsvMap (csvMap @r)
  where fromCsvMap (CsvMap mapping) = hFoldMap @_ @C.Header id mapping

-- | Tapioca equivalent of cassava's toRecord
toRecord :: forall r. CsvMapped r => r -> C.Record
toRecord record = foldCsvMap (csvMap @r)
  where foldCsvMap (CsvMap mapping) = hFoldMap @_ @(Reader r (V.Vector C.Field)) (`runReader` record) mapping

-- | Tapioca equivalent of cassava's toNamedRecord
toNamedRecord :: CsvMapped r => r -> C.NamedRecord
toNamedRecord = undefined

type GenericCsvDecode r t i = (GParseRecord (Rep r) r t i, Generic r)

data CsvMap r = forall m.
  ( GenericCsvDecode r m C.NamedRecord
  , GenericCsvDecode r m C.Record
  , HFoldable m (Reader r (V.Vector C.Field)) -- For encode ToRecord
  , HFoldable m C.Header -- To List headers
  , Width m
  ) => CsvMap m

-- Our joining/induction type for records
infixl 1 :|
data a :| b = a :| b

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

  (<->) :: forall s f c. (C.FromField c, C.ToField c) => B.ByteString -> Codec s r f c -> FieldMapping s r f
  name <-> cdc  = Field name cdc

-- | The 'link' in a mapping chain.
data FieldMapping (s :: Symbol) r f where
  Field :: forall s r f c. (C.FromField c, C.ToField c) => B.ByteString -> Codec s r f c -> FieldMapping s r f
  Splice :: forall s r f c. (CsvMapped c, Generic c) => Codec s r f c -> FieldMapping s r f

-- | Heterogeneous folding required for encoding
class HFoldVal (t :: Type) x where
  hFoldVal :: t -> x

class HFoldable (t :: Type) x where
  hFoldr :: (x -> b -> b) -> b -> t -> b
  hFoldMap :: Semigroup m => (x -> m) -> t -> m
  hFoldl :: (b -> x -> b) -> b -> t -> b

-- | Basic fold instance
instance {-# Overlappable #-} HFoldVal t x => HFoldable t x where
  hFoldl f b x = f b (hFoldVal x)
  hFoldr f b x = f (hFoldVal x) b
  hFoldMap f x = f (hFoldVal x)

-- | Induction over :|
instance {-# Overlapping #-} (HFoldable t x, HFoldable ts x) => HFoldable (t :| ts) x where
  hFoldl f b (x :| xs) = hFoldl f (hFoldl f b x) xs
  hFoldr f b (x :| xs) = hFoldr f (hFoldr f b  x) xs
  hFoldMap f (x :| xs) = hFoldMap f x <> hFoldMap f xs

-- | Support for encoding
instance HFoldVal (FieldMapping s r f) (Reader r (V.Vector C.Field)) where
  hFoldVal fm = case fm of
    Field _ cdc -> asks $ V.singleton . C.toField . biTo (_codec cdc) . _getCodecField cdc
    Splice cdc -> asks $ toRecord . biTo (_codec cdc) . _getCodecField cdc

-- | Generate a header entry for this mapping
instance HFoldVal (FieldMapping s r f) C.Header where
  hFoldVal (Field name _) = pure name
  hFoldVal (Splice (_ :: Codec s r f c)) = hFoldOf (csvMap @c)
    where hFoldOf (CsvMap (m :: t)) = hFoldMap @_ @C.Header id m

-- | Type family to determine whether an arbitrary selector matches that of our mapping
type family Match t s :: Bool where
  Match (FieldMapping s _ _) s' = EqSymbol s s'
  Match (t1 :| t2) s = Match t1 s || Match t2 s

type family OrdBool (o :: Ordering) :: Bool where
  OrdBool 'LT = 'False
  OrdBool 'EQ = 'True
  OrdBool 'GT = 'False

type EqSymbol s s' = OrdBool (CmpSymbol s s')

-- Return t1 if provided with true, otherwise t2
class PickMatch (t1 :: Type) (t2 :: Type) (b :: Bool) where
  type Picked t1 t2 b
  picked :: t1 :| t2 -> Picked t1 t2 b

instance PickMatch t1 t2 'True where
  type Picked t1 t2 'True = t1
  picked (t1 :| _) = t1

instance PickMatch t1 t2 'False where
  type Picked t1 t2 'False = t2
  picked (_ :| t2) = t2

-- | Class for terms which can be reduced to a simpler type
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) r f where
  selectorMapping :: t -> FieldMapping s r f

-- | Base case
instance (r~r', f~f') => Reduce (FieldMapping s r f) s r' f' where
  selectorMapping = id

-- | Inductive case
instance (Reduce tt s r f, m ~ Match t1 s, PickMatch t1 t2 m, tt ~ Picked t1 t2 m) => Reduce (t1 :| t2) s r f where
 selectorMapping t = selectorMapping (picked @_ @_ @m t)

-- | Determine how many columns a mapping consumes
-- Splices may take > 1
class Width t where
  width :: t -> Int

instance Width (FieldMapping s r f) where
  width (Field _ _) = 1
  width (Splice (_ :: Codec _ _ _ c)) = widthOf (csvMap @c)
    where widthOf (CsvMap mapping) = width mapping

instance (Width t1, Width t2) => Width (t1 :| t2) where
  width (t1 :| t2) = width t1 + width t2

-- | Class for looking up position of selector in our type
-- Takes into consideration splices inserted before position
class Index t (s :: Symbol) where
  index :: t -> Int

instance Index (FieldMapping s r f) s where
  index _ = 0
  
-- | Class to decide on wether to progress to next segment based on selector matching of first
class PickNext (t1 :: Type) (t2 :: Type) (m :: Bool) where
  type Next t1 t2 m :: Type
  incr :: t1 -> Int
  next :: t1 -> t2 -> Next t1 t2 m

instance PickNext t1 t2 'True where
  type Next t1 t2 'True = t1
  incr _ = 0
  next t1 _ = t1 

instance Width t1 => PickNext t1 t2 'False where
  type Next t1 t2 'False = t2
  incr = width
  next _ t2 = t2
  
instance (m ~ Match t1 s, PickNext t1 t2 m, Index (Next t1 t2 m) s) => Index (t1 :| t2) s where
  index (t1 :| t2) = incr @_ @t2 @m t1 + index @_ @s (next @_ @_ @m t1 t2)
  
-- | Generic creation of record from CsvMap
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
    where parseByType = case selectorMapping @_ @s @r @f fieldMapping of
            Field name cdc -> maybe (fail errMsg) decode val
              where errMsg = "No column " <> BC.unpack name <> " in columns: " <> bsVectorString (HM.keys namedRecord)
                    val = HM.lookup name namedRecord
                    decode = (biFrom (_codec cdc) <$>) . C.parseField
            Splice (cdc :: Codec s r _ c) -> parseSplice (csvMap @c)
              where parseSplice (CsvMap cm) = biFrom (_codec cdc) . to <$> gParseRecord @_ @c proxy# cm namedRecord

instance (Reduce t s r f, Index t s) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @r @f fieldMapping of
            Field _ cdc -> maybe (fail errMsg) decode val
              where errMsg = "Can't parse item at index " <> show (index @_ @s fieldMapping) <> " in row: " <> bsVectorString (V.toList record)
                    decode = (biFrom (_codec cdc) <$>) . C.parseField
                    --idx = index
                    val = record V.!? index @_ @s fieldMapping
            Splice (cdc :: Codec s r _ c) -> parseSplice (csvMap @c)
              where parseSplice (CsvMap cm) = biFrom (_codec cdc) . to <$> gParseRecord @_ @c proxy# cm record

instance (GParseRecord a r t i, GParseRecord b r t i) => GParseRecord (a :*: b) r t i where
  gParseRecord p t mapping = do
    a <- gParseRecord p t mapping
    b <- gParseRecord p t mapping
    pure $ a :*: b
