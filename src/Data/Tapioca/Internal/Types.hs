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

module Data.Tapioca.Internal.Types
  ( CsvMap(..)
  ,  CsvMapped(..)
  , Codec(..)
  , FieldMapping(..)
  , Header(..)
  , (:|)(..)
  , LookupFieldMapping(..)
  , GenericCsvDecode(..)
  , GParseRecord(..)
  ) where

import GHC.Records
import GHC.TypeLits
import GHC.Exts
import GHC.Generics
import GHC.OverloadedLabels

import Data.Kind
import qualified Data.Csv as C
import qualified Data.ByteString as B
import qualified Data.Profunctor as P

data CsvMap r = forall m. GenericCsvDecode r m => CsvMap m

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

infixl 1 :|
data a :| b = a :| b

infixl 2 :=
data FieldMapping s r f d e = B.ByteString := (Codec r f d e)
                            | Splice (Codec r f d e)

instance (HasField x r f, f ~ d, f ~ e, C.ToField e, C.FromField d) => IsLabel x (Codec r f d e) where
  fromLabel = Codec (getField @x) id

instance (HasField x r f, f ~ d, f ~ e, CsvMapped f) => IsLabel x (FieldMapping x r f d e) where
  fromLabel = Splice $ Codec (getField @x) id

-- t - type which contains our mapping somewhere
-- s - selector name
-- r - record
-- f - field / selector type
-- d - decode as type
-- e - encode as type
-- lookup result
class LookupFieldMapping t s r f d e (v :: Bool) | t s r f d e -> v where
  selectorMapping :: t -> FieldMapping s r f d e

type family OrdBool (b :: Ordering) where
  OrdBool 'LT = 'False
  OrdBool 'EQ = 'True
  OrdBool 'GT = 'False

 -- Base case
instance (HasField s r f,v ~ OrdBool (CmpSymbol s s'),s~s',r~r',f~f',d~d',e~e') => LookupFieldMapping (FieldMapping s r f d e) s' r' f' d' e' v where
  selectorMapping = id

data SSide sv fv dv ev t1 s1 f1 d1 e1 t2 s2 f2 d2 e2 where
  SLeft :: (sv ~ s1, fv ~ f1, dv ~ d1, ev ~ e1) => SSide sv fv dv ev t1 s1 f1 d1 e1 t2 s2 f2 d2 e2
  SRight :: (sv ~ s2, fv ~ f2, dv ~ d2, ev ~ e2) => SSide sv fv dv ev t1 s1 f1 d1 e1 t2 s2 f2 d2 e2

class LookupSide sv fv dv ev t1 s1 f1 d1 e1 t2 s2 f2 d2 e2 (sd :: SSide sv fv dv ev t1 s1 f1 d1 e1 t2 s2 f2 d2 e2) where
  side :: Proxy# sd -> SSide sv fv dv ev t1 s1 f1 d1 e1 t2 s2 f2 d2 e2

data Match (sd :: SSide sv fv dv ev t1 s1 f1 d1 e1 t2 s2 f2 d2 e2) t s f d e (v :: Bool)

instance
  ( LookupFieldMapping t1 s1 r1 f1 d1 e1 v1
  , LookupFieldMapping t2 s2 r2 f2 d2 e2 v2
  , r1 ~ r2
  , MatchMapping s1 f1 d1 e1 v1 s2 f2 d2 e2 v2 ~ Match sd sv fv dv ev vv
  , LookupSide sv fv dv ev t1 s1 f1 d1 e1 t2 s2 f2 d2 e2 sd
  )  => LookupFieldMapping (t1 :| t2) sv rv fv dv ev vv where
  selectorMapping (t1 :| t2) = case side @sv @fv @dv @ev @t1 @s1 @f1 @d1 @e1 @t2 @s2 @f2 @d2 @e2 (proxy# :: Proxy# sd) of
    SLeft -> selectorMapping @t1 @s1 @r1 @f1 @d1 @e1 @v1 t1
    SRight -> selectorMapping @t2 @s2 @r2 @f2 @d2 @e2 @v2 t2

type family MatchMapping s1 f1 d1 e1 (v1 :: Bool) s2 f2 d2 e2 (v2 :: Bool) where
  MatchMapping s1 f1 d1 e1 'True _ _ _ _ _ = Match 'SLeft s1 f1 d1 e1 'True
  MatchMapping _ _ _ _ 'False s2 f2 d2 e2 'True = Match 'SRight s2 f2 d2 e2 'True
  MatchMapping _ f1 _ _ 'False _ f2 _ _ 'False = TypeError ('Text "Types " ':<>: 'ShowType f1 ':<>: 'Text " and " ':<>: 'ShowType f2 ':<>: 'Text " dont't match")

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
data Header = WithHeader | WithoutHeader


type GenericCsvDecode r t = (GParseRecord (Rep r) r t, Generic r)

-- r :: record type we are parsing to
-- t :: Our smap type
-- namedRecord :: type level map (selector -> data) (
class GParseRecord (f :: Type -> Type) r t where
  gParseRecord :: Proxy# r -> t -> C.NamedRecord -> C.Parser (f p)

instance GParseRecord f r t => GParseRecord (M1 D x f) r t where
  gParseRecord p fieldmaps nr = M1 <$> gParseRecord p fieldmaps nr

instance GParseRecord f r t => GParseRecord (M1 C x f) r t where
  gParseRecord p fieldmaps nr = M1 <$> gParseRecord p fieldmaps nr

-- d and e are the floating variables here, should be able to infer them from the fundeps
-- might need to split into two typeclasses if it can't resolve
instance LookupFieldMapping t s r f d e 'True => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3i) (K1 i a)) r t where
  gParseRecord _ fieldMaps nr = M1 . K1 <$> fail ""

instance (GParseRecord a r t, GParseRecord b r t) => GParseRecord (a :*: b) r t where
  gParseRecord p sm mapping = do
    a <- gParseRecord p sm mapping
    b <- gParseRecord p sm mapping
    pure $ a :*: b
