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
{-# LANGUAGE InstanceSigs #-}

module Data.Tapioca.Internal.Types
  ( CsvMap(..)
  ,  CsvMapped(..)
  , Codec(..)
  , FieldMapping(..)
  , Header(..)
  , (:|)(..)
  , GenericCsvDecode(..)
  , GParseRecord(..)
  ) where

import GHC.Records
import GHC.TypeLits
import GHC.Exts
import GHC.Generics
import GHC.OverloadedLabels

import Data.Kind
import Data.Type.Bool
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
data FieldMapping (s :: Symbol) r f d e = B.ByteString := (Codec r f d e)
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
--class LookupFieldMapping t s r f d e (v :: Bool) | t s r -> f d e v where
--  selectorMapping :: t -> FieldMapping s r f d e

type family OrdBool (o :: Ordering) :: Bool where
  OrdBool 'LT = 'False
  OrdBool 'EQ = 'True
  OrdBool 'GT = 'False

class Reduce t (s :: Symbol) tt | t s -> tt where
  type Match t s :: Bool
  selectorMapping :: t -> tt

-- class OrdMaybe (o :: Ordering) t where val :: t -> Maybe t
-- instance OrdMaybe 'LT t where val _ = Nothing
-- instance OrdMaybe 'EQ t where val t = Just t
-- instance OrdMaybe 'GT t where val _ = Nothing

  -- Base case
instance HasField s r f => Reduce (FieldMapping s' r f d e) s (FieldMapping s' r f d e) where
  type Match (FieldMapping s' r f d e) s = OrdBool (CmpSymbol s s')
  selectorMapping = id

--Typeclass for determining which element of some alternatives matches our selector
--class FindFieldMapping (t :: k) (s :: Symbol) where
--  type Result t s :: k

--instance FindFieldMapping (t1 :| t2) s where
  -- type Result (t1 :| t2) s = If (Match t1 s) t1
  --                   (If (Match t2 s) t2
  --                   (TypeError (Text "Types " :<>: ShowType t1 :<>: 'Text " and " ':<>: 'ShowType t2 ':<>: 'Text " dont't contain selector " ':<>: 'ShowType s)))
  -- type Result (t1 :| t2) s = If (Match t1 s) t1 t2
--  type Result (t1 :| t2) s = t1

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
  selectorMapping :: (t1 :| t2) -> tv
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

instance Reduce t s (FieldMapping s r f d e) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i a)) r t where
  gParseRecord _ fieldMaps nr = M1 . K1 <$> fail ""

instance (GParseRecord a r t, GParseRecord b r t) => GParseRecord (a :*: b) r t where
  gParseRecord p sm mapping = do
    a <- gParseRecord p sm mapping
    b <- gParseRecord p sm mapping
    pure $ a :*: b
