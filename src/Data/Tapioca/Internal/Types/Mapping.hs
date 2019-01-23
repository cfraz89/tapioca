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
{-# LANGUAGE GADTs #-}

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
import qualified Data.HashMap.Strict as HM
import Data.Kind

type GenericCsvDecode r t i = (GParseRecord (Rep r) r t i, Generic r)

data CsvMap r = forall m.
  ( GenericCsvDecode r m C.NamedRecord
  , GenericCsvDecode r m C.Record
  ) => CsvMap m

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

  (<->) :: forall s f. (HasField s r f, C.FromField f) => B.ByteString -> SelectorProxy s -> FieldMapping s r f
  bs <-> _ = Field bs idCodec

data FieldMapping (s :: Symbol) r f =
  forall d e. C.FromField d => Field B.ByteString (Codec s r f d e)
  | forall d e m. (CsvMapped d, Generic d) => Splice (Proxy# m) (Codec s r f d e)



instance (HasField x r f, f ~ d, f ~ e, CsvMapped f, Generic f, Generic e, GenericCsvDecode d m C.NamedRecord) => IsLabel x (FieldMapping x r f) where
  fromLabel = Splice @x @r @f @d @e (proxy# @m) (Codec (getField @x) id)

-- Types that get joined
--infixl 2 (=>)
data SelectorProxy (s :: Symbol) = SelectorProxy

instance (s~s') => IsLabel s (SelectorProxy s') where
  fromLabel = SelectorProxy


-- Class for terms which can be reduced to a simpler type
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) r f where
  --type Match t s :: Bool
  selectorMapping :: t -> FieldMapping s r f

-- Base case
instance (r~r', f~f') => Reduce (FieldMapping s r f) s r' f' where
  --type Match (FieldMapping s' r f d e) s = EqSymbol s' s
  selectorMapping = id

instance Reduce (FieldMapping s r f :| ts) s r f where
  selectorMapping (t :| _) = t

--instance {-# Overlapping #-} Reduce ts s r f d e => Reduce (FieldMapping s' r' f' d' e' :| ts) s r f d e where
--  selectorMapping (_ :| ts) = selectorMapping ts


-- Inductive case
--instance (Reduce tt s r f d e, m ~ Match t1 s, PickMatch t1 t2 m, tt ~ Picked t1 t2 m, t1 ~ FieldMapping s r f d e) => Reduce (t1 :| t2) s r f d e where
--  type Match (t1 :| t2) s = Match t1 s || Match t2 s
--  selectorMapping t = selectorMapping @tt @s (picked @t1 @t2 @m t)

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
  gParseRecord _ fieldMapping nr = M1 . K1 <$> parseByType
    where parseByType :: C.Parser f
          parseByType = case selectorMapping @t @s fieldMapping of
            (Field name (fm :: Codec s r f d e)) -> maybe (fail $ "No column " <> BC.unpack name <> " in header") ((decoder fm <$>) . C.parseField @d) (HM.lookup name nr)
            (Splice _ (fm :: Codec s r f d e)) -> parseSplice (csvMap @d)
              where parseSplice :: CsvMap d -> C.Parser f
                    parseSplice (CsvMap (m :: m)) = decoder fm . to <$> gParseRecord @_ @d @m proxy# m nr

-- TODO
instance Reduce t s r f => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping nr = M1 . K1 <$> undefined

instance (GParseRecord a r t1 i, GParseRecord b r t2 i) => GParseRecord (a :*: b) r (t1 :| t2) i where
  gParseRecord p (sm :| sms) mapping = do
    a <- gParseRecord p sm mapping
    b <- gParseRecord p sms mapping
    pure $ a :*: b
