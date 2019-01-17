{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeInType #-}

module Data.Tapioca.Internal.Types 
  ( CsvMap(..)
  , CsvMapped(..)
  , Codec(..)
  , FieldMapping(..)
  , Header(..)
  , (:|)(..)
  , LookupFieldMapping(..)
  ) where

import GHC.Records
import GHC.TypeLits
import GHC.Exts

import qualified Data.ByteString as B
import Data.Profunctor

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r l where
  csvMap :: CsvMap r l


-- l is some kind of MappingList
newtype CsvMap r l = CsvMap { unCsvMap :: l }
  deriving (Show, Semigroup, Monoid)

infixl 0 :|
data a :| b = a :| b

infixl 1 :=

data FieldMapping s r f d e = B.ByteString := Codec r f d e
                            | Splice (Codec r f d e)


-- t - type which contains our mapping somewhere
-- s - selector name
-- r - record
-- f - field / selector type
-- d - decode as type
-- e - encode as type
-- lookup result
class LookupFieldMapping t s r f d e (v :: Bool) | t s r -> f d e v where  
  selectorMapping :: t -> FieldMapping s r f d e

 -- Base case
instance HasField s r f => LookupFieldMapping (FieldMapping s r f d e) s r f d e 'True where
  selectorMapping = id

data SSide fv dv ev t1 f1 d1 e1 t2 f2 d2 e2 where
  SLeft :: (fv ~ f1, dv ~ d1, ev ~ e1) => SSide fv dv ev t1 f1 d1 e1 t2 f2 d2 e2
  SRight :: (fv ~ f2, dv ~ d2, ev ~ e2) => SSide fv dv ev t1 f1 d1 e1 t2 f2 d2 e2

class LookupSide fv dv ev t1 f1 d1 e1 t2 f2 d2 e2 (sd :: SSide fv dv ev t1 f1 d1 e1 t2 f2 d2 e2) where
  side :: Proxy# sd -> SSide fv dv ev t1 f1 d1 e1 t2 f2 d2 e2 

data Match (sd :: SSide fv dv ev t1 f1 d1 e1 t2 f2 d2 e2) t f d e (v :: Bool)

instance
  ( LookupFieldMapping t1 s r f1 d1 e1 v1
  , LookupFieldMapping t2 s r f2 d2 e2 v2
  , MatchMapping f1 d1 e1 v1 f2 d2 e2 v2 ~ Match sd fv dv ev vv
  , LookupSide fv dv ev t1 f1 d1 e1 t2 f2 d2 e2 sd
  )  => LookupFieldMapping (t1 :| t2) s r fv dv ev vv where
  selectorMapping (t1 :| t2) = case side @fv @dv @ev @t1 @f1 @d1 @e1 @t2 @f2 @d2 @e2 (proxy# :: Proxy# sd) of
    SLeft -> selectorMapping t1
    SRight -> selectorMapping t2

type family MatchMapping f1 d1 e1 (v1 :: Bool) f2 d2 e2 (v2 :: Bool) where
  MatchMapping f1 d1 e1 'True _ _ _ _ = Match SLeft f1 d1 e1 'True
  MatchMapping _ _ _ 'False f2 d2 e2 'True = Match SRight f2 d2 e2 'True
  MatchMapping f1 _ _ 'False f2 _ _ 'False = TypeError (Text "Types " :<>: ShowType f1 :<>: Text " and " :<>: ShowType f2 :<>: Text " dont't match") 

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- d - Type to decode as
-- e - type to encode as
data Codec r f d e = Codec
  { encoder :: r -> e
  , decoder :: d -> f
  }

instance Profunctor (Codec r f) where
  dimap d e fm = fm
    { encoder = e . encoder fm
    , decoder = decoder fm . d
    }

-- | When encoding, whether or not to write the header row.\n
-- When decoding, whether or not the csv being decoded contains a header row.\n
-- if decoding WithoutHeader, tapioca will map the order of fields in the csv
-- to the order that fields are specified in the csvMap.
data Header = WithHeader | WithoutHeader
