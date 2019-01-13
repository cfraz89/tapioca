{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Data.Tapioca.Internal.Types 
  ( CsvMap(..)
  , CsvMapped(..)
  , SelectorMapping(..)
  , FieldMapping(..)
  , Header(..)
  ) where

import Data.Tapioca.Internal.Decode.Generic (GenericCsvDecode)

import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Records

import qualified Data.ByteString as B
import qualified Data.Csv as C
import Data.Profunctor
import Data.Proxy
import qualified Data.Vector as V
import Type.Reflection

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

newtype CsvMap r = CsvMap { unCsvMap :: V.Vector (SelectorMapping r) }
  deriving (Show, Semigroup, Monoid)

infixl 0 :=
data SelectorMapping r = forall x i f e d. (C.ToField e, C.FromField d, Typeable f) => B.ByteString := FieldMapping x i r f d e
                       | forall x i f e d. (GenericCsvDecode d, Typeable f, CsvMapped f, CsvMapped e, CsvMapped d) => Splice (FieldMapping x i r f d e)

instance Show (SelectorMapping r) where
  show (name := fm) = show name <> " := "
  show (Splice fm) = "Splice"

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- e - type to encode as
-- g - Type to decode as
data FieldMapping (x :: Symbol) (i :: Nat) r f d e = FieldMapping
  { encoder :: r -> e
  , decoder :: d -> f
  }

instance Profunctor (FieldMapping x i r f) where
  dimap d e fm = fm
    { encoder = e . encoder fm
    , decoder = decoder fm . d
    }

instance (HasField x r f, IndexedField x r i, f ~ d, f ~ e, x~x1) => IsLabel x (FieldMapping x1 i r f d e) where
  fromLabel = FieldMapping @x @i (getField @x) id

instance (HasField x r f, IndexedField x r i, f ~ d, f ~ e, Typeable f, CsvMapped f, GenericCsvDecode f)
  => IsLabel x (SelectorMapping r) where
    fromLabel = Splice $ FieldMapping @x @i (getField @x) (id :: d -> f)

instance KnownSymbol x => Show (FieldMapping x i r f d e) where
  show fm = "Mapping " <> symbolVal' @x proxy#

class IndexedField (x :: Symbol) r (i :: Nat) | x r -> i, i r -> x

-- | When encoding, whether or not to write the header row.\n
-- When decoding, whether or not the csv being decoded contains a header row.\n
-- if decoding WithoutHeader, tapioca will map the order of fields in the csv
-- to the order that fields are specified in the csvMap.
data Header = WithHeader | WithoutHeader
