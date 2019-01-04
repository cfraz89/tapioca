{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tapioca.Types 
  ( CsvMap(..)
  , CsvMapped(..)
  , SelectorMapping(..)
  , FieldMapping(..)
  , Header(..)
  ) where

import Data.Tapioca.Internal.Decode.Generic (GenericCsvDecode)

import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Records

import qualified Data.ByteString as B
import qualified Data.Csv as C
import Data.Proxy
import qualified Data.Vector as V
import Type.Reflection

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

newtype CsvMap r = CsvMap { unCsvMap :: V.Vector (SelectorMapping r) }
  deriving (Show, Semigroup, Monoid)

infixl 0 :=
data SelectorMapping r = forall f e d. (C.ToField e, C.FromField d, Typeable f) => B.ByteString := FieldMapping r f e d
                       | forall f e d. (GenericCsvDecode d, Typeable f, CsvMapped f, CsvMapped e, CsvMapped d) => Splice (FieldMapping r f e d)

instance Show (SelectorMapping r) where
  show (name := fm) = show name <> " := " <> show fm
  show (Splice fm) = "Splice " <> show fm

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- e - type to encode as
-- g - Type to decode as
data FieldMapping r f e d = FieldMapping
  { selector :: String
  , encoder :: r -> e
  , decoder :: d -> f
  }

instance (HasField x r f, KnownSymbol x, f ~ d, f ~ e) => IsLabel x (FieldMapping r f e d) where
  fromLabel = FieldMapping (symbolVal @x Proxy) (getField @x) id

instance (HasField x r f, KnownSymbol x, f ~ d, f ~ e, Typeable f, CsvMapped f, GenericCsvDecode f) => IsLabel x (SelectorMapping r) where
  fromLabel = Splice $ FieldMapping (symbolVal @x Proxy) (getField @x) (id :: d -> f)

instance Show (FieldMapping r f e d) where
  show fm = "Mapping " <> selector fm

-- | When encoding, whether or not to write the header row
-- When decoding, whether or not the csv being decoded contains a header row
-- if decoding WithoutHeader, tapioca will map the order of fields in the csv
-- to the order that fields are specified in the csvMap.
data Header = WithHeader | WithoutHeader

