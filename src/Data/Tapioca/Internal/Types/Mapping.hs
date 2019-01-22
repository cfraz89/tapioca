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
{-# LANGUAGE FunctionalDependencies #-}

module Data.Tapioca.Internal.Types.Mapping where

import GHC.Exts
import GHC.Generics
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

import Data.Tapioca.Internal.Types.Codec
import Data.Tapioca.Internal.Types.Sep

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import Data.Kind
import qualified Data.HashMap.Strict as HM
import Data.Type.Bool

type GenericCsvDecode r t i = (GParseRecord (Rep r) r t i, Generic r)

data CsvMap r = forall m.
  ( GenericCsvDecode r m C.NamedRecord
  , GenericCsvDecode r m C.Record
  ) => CsvMap m


-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

instance (HasField x r f, f ~ d, f ~ e, CsvMapped f, Generic f, Generic e, GenericCsvDecode d m C.NamedRecord) => IsLabel x (FieldMapping x r f d e) where
  fromLabel = Splice (proxy# @m) (Codec (getField @x) id)

-- Types that get joined
infixl 2 :=>
data FieldMapping (s :: Symbol) r f d e
  = C.FromField d => B.ByteString :=> (Codec r f d e)
  | forall m. (CsvMapped d, Generic d) => Splice (Proxy# m) (Codec r f d e)

-- Class for terms which can be reduced to a simpler type
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) r f d e  | t s -> r f d e where
  type Match t s :: Bool
  selectorMapping :: t -> FieldMapping s r f d e

-- Base case
instance HasField s r f => Reduce (FieldMapping s' r f d e) s r f d e where
  type Match (FieldMapping s' r f d e) s = EqSymbol s' s
  selectorMapping (n :=> fm) = n :=> fm
  selectorMapping (Splice p s) = Splice p s

-- Inductive case
instance (Reduce tt s r f d e, m ~ Match t1 s, PickMatch t1 t2 m, tt ~ Picked t1 t2 m, t1 ~ FieldMapping s r f d e) => Reduce (t1 :| t2) s r f d e where
  type Match (t1 :| t2) s = Match t1 s || Match t2 s
  selectorMapping t = selectorMapping @tt @s (picked @t1 @t2 @m t)

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

instance (Reduce t s r f d e)  => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping nr = M1 . K1 <$> parseByType
    where parseByType :: C.Parser f
          parseByType = case selectorMapping @t @s fieldMapping of
            (name :=> fm) -> maybe (fail $ "No column " <> BC.unpack name <> " in header") ((decoder fm <$>) . C.parseField @d) (HM.lookup name nr)
            (Splice _ (fm :: Codec r' f' d' e')) -> parseSplice (csvMap @d)
              where parseSplice :: CsvMap d -> C.Parser f
                    parseSplice (CsvMap (m :: m)) = decoder fm . to <$> gParseRecord @_ @d @m proxy# m nr

-- TODO
instance Reduce t s r f d e => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping nr = M1 . K1 <$> undefined

instance (GParseRecord a r t1 i, GParseRecord b r t2 i) => GParseRecord (a :*: b) r (t1 :| t2) i where
  gParseRecord p (sm :| sms) mapping = do
    a <- gParseRecord p sm mapping
    b <- gParseRecord p sms mapping
    pure $ a :*: b
