{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Data.Tapioca.Internal.Types
  ( CsvMap(..)
  ,  CsvMapped(..)
  , Codec(..)
  , FieldMapping(..)
  , EncodeIndexing(..)
  , DecodeIndexing(..)
  , (:|)(..)
  , GenericCsvDecode
  , GParseRecord(..)
  --, HFoldable(..)
  ) where

import GHC.Records
import GHC.TypeLits
import GHC.Exts
import GHC.Generics
import GHC.OverloadedLabels

import Data.Proxy
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import Data.Kind
import qualified Data.Profunctor as P
import Data.Type.Bool

data CsvMap r = forall m. GenericCsvDecode r m C.NamedRecord => CsvMap m

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

infixl 1 :|
data a :| b = a :| b

--class HFoldable (t :: Type) where

  -- hFoldr :: (forall x. x -> b -> b) -> b -> t -> b
  -- hFoldMap :: Monoid m => (forall x. x -> m) -> t -> m

  -- hFoldl :: (b -> FieldMapping t s r f d e -> b) -> b -> (FieldMapping t s r f d e :| xs) -> b
  -- hFoldl f b (x :| xs) = hFoldl f (f b x) xs
  -- hFoldr f b (x :| xs) = hFoldr f (f x b) xs
  -- hFoldMap f (x :| xs) = f x <> hFoldMap f xs

infixl 2 :=
data FieldMapping (s :: Symbol) r f d e
  = C.FromField d => B.ByteString := (Codec r f d e)
  | forall m. (CsvMapped d, Generic d) => Splice (Proxy m) (Codec r f d e)

instance (HasField x r f, f ~ d, f ~ e, C.ToField e, C.FromField d) => IsLabel x (Codec r f d e) where
  fromLabel = Codec (getField @x) id

instance (HasField x r f, f ~ d, f ~ e, CsvMapped f, Generic f, Generic e, GenericCsvDecode d m C.NamedRecord) => IsLabel x (FieldMapping x r f d e) where
  fromLabel = Splice (Proxy @m) (Codec (getField @x) id)

type family OrdBool (o :: Ordering) :: Bool where
  OrdBool 'LT = 'False
  OrdBool 'EQ = 'True
  OrdBool 'GT = 'False

type EqSymbol s s' = OrdBool (CmpSymbol s s')

-- Class for terms which can be reduced to a simpler type
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) tt | t s -> tt where
  type Match t s :: Bool
  selectorMapping :: t -> tt

  -- Base case
instance HasField s r f => Reduce (FieldMapping s' r f d e) s (FieldMapping s' r f d e) where
  type Match (FieldMapping s' r f d e) s = EqSymbol s s'
  selectorMapping = id

class PickMatch (t1 :: Type) (t2 :: Type) (b :: Bool) where
  type Picked t1 t2 b
  picked :: t1 :| t2 -> Picked t1 t2 b

instance PickMatch t1 t2 'True where
  type Picked t1 t2 'True = t1
  picked (t1 :| _) = t1

instance PickMatch t1 t2 'False where
  type Picked t1 t2 'False = t2
  picked (_ :| t2) = t2

instance (Reduce tt s tv, m ~ Match t1 s, PickMatch t1 t2 m, tt ~ Picked t1 t2 m) => Reduce (t1 :| t2) s tv where
  type Match (t1 :| t2) s = Match t1 s || Match t2 s
  selectorMapping t = selectorMapping @_ @s (picked @_ @_ @m t)

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- d - Type to decode as
-- e - type to encode as
data Codec r f d e = Codec
  { encoder :: r -> e
  , decoder :: d -> f
  }

instance P.Profunctor (Codec r f) where
  dimap d e fm = fm
    { encoder = e . encoder fm
    , decoder = decoder fm . d
    }

-- | When encoding, whether or not to write the header row.\n
-- When decoding, whether or not the csv being decoded contains a header row.\n
-- if decoding WithoutHeader, tapioca will map the order of fields in the csv
-- to the order that fields are specified in the csvMap.

data DecodeIndexing r t where
  DecodeNamed :: C.FromNamedRecord r => DecodeIndexing r C.NamedRecord
  DecodeOrdered :: C.FromRecord r => C.HasHeader -> DecodeIndexing r C.Record

data EncodeIndexing r t where
  EncodeNamed :: C.ToNamedRecord r => EncodeIndexing r C.NamedRecord
  EncodeOrdered :: C.ToRecord r => EncodeIndexing r C.NamedRecord

type GenericCsvDecode r t i = (GParseRecord (Rep r) r t i, Generic r)

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

instance Reduce t s (FieldMapping s r f d e) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping nr = M1 . K1 <$> parseByType
    where parseByType :: C.Parser f
          parseByType = case selectorMapping @t @s fieldMapping of
            (name := fm) -> maybe (fail $ "No column " <> BC.unpack name) ((decoder fm <$>). C.parseField @d) (HM.lookup name nr)
            (Splice _ (fm :: Codec r' f' d' e')) -> parseSplice (csvMap @d)
              where parseSplice :: CsvMap d -> C.Parser f
                    parseSplice (CsvMap (m :: m)) = decoder fm . to <$> gParseRecord @_ @d @m proxy# m nr

-- TODO
--instance Reduce t s (FieldMapping s r f d e) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
--    gParseRecord _ fieldMapping nr = M1 . K1 <$> undefined

instance (GParseRecord a r t i, GParseRecord b r t i) => GParseRecord (a :*: b) r t i where
  gParseRecord p sm mapping = do
    a <- gParseRecord p sm mapping
    b <- gParseRecord p sm mapping
    pure $ a :*: b
