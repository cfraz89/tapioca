{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | Functions needed in both encoding and decoding
module Data.Tapioca.Internal.Common
  ( header
  , (?!)
  , toParser
  , parseWithCsvMap
  , DecodeIndexing(..)
  ) where

import Data.Tapioca.Internal.Types.Mapping

import GHC.Exts
import GHC.Generics

import Control.Error.Util
import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.Vector as V

-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- | Similar to cassava's headerOrder function
header :: forall r. CsvMapped r => V.Vector B.ByteString
header = undefined
-- header = fromCsvMap (csvMap @r)
--  where fromCsvMap (CsvMap t) = foldOn t
--        foldOn t@(x :| xs) = hFoldMap allNames t
--        allNames (name := _) = pure name
--        allNames (Splice (_ :: Codec r f d e)) = header @f

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

toParser :: Either String a -> C.Parser a
toParser (Left e) = fail e
toParser (Right a) = pure a

data DecodeIndexing r t where
  DecodeNamed :: DecodeIndexing r C.NamedRecord -- assumes presence of header
  DecodeOrdered :: C.HasHeader -> DecodeIndexing r C.Record

class ParseWithCsvMap r t where 
  parseWithCsvMap :: CsvMapped r => t -> C.Parser r

instance Generic r => ParseWithCsvMap r C.NamedRecord where
  parseWithCsvMap namedRecord = parseFrom csvMap
    where parseFrom (CsvMap m :: _ r) = to <$> gParseRecord @_ @r proxy# m namedRecord

instance ParseWithCsvMap r C.Record where
  parseWithCsvMap record = parseFrom csvMap
    where parseFrom (CsvMap m :: _ r) = to <$> gParseRecord @_ @r proxy# m record
