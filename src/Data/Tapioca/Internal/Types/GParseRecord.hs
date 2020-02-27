{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Tapioca.Internal.Types.GParseRecord where

import qualified Data.Csv as C
import Data.Kind

import GHC.Exts
import GHC.Generics

type GenericCsvDecode r t i = (GParseRecord (Rep r) r t i, Generic r)

-- | Generic creation of record from CsvMap
-- f :: Generic representation
-- r :: record type we are parsing to
-- t :: Our CsvMap unwrapped type
-- i :: Indexing type - Record or NamedRecord
class GParseRecord (f :: Type -> Type) r t i where
  gParseRecord :: Proxy# r -> t -> i -> C.Parser (f p)

instance GParseRecord f r t i => GParseRecord (D1 x f) r t i where
  gParseRecord p fieldMapping record = M1 <$> gParseRecord p fieldMapping record

-- Only dig down to single field generic when it has more than one selector, allowing us to use coerce without overlap
instance GParseRecord (a :*: b) r t i => GParseRecord (C1 x (a :*: b)) r t i where
  gParseRecord p fieldMapping record = M1 <$> gParseRecord p fieldMapping record

instance (GParseRecord a r t i, GParseRecord b r t i) => GParseRecord (a :*: b) r t i where
  gParseRecord p t mapping = do
    a <- gParseRecord p t mapping
    b <- gParseRecord p t mapping
    pure $ a :*: b
