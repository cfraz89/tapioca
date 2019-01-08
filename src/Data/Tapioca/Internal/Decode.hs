{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Data.Tapioca.Internal.Decode
  ( parseRecord
  , eitherParser
  , parseNamedRecord
  ) where 

import Data.Tapioca.Internal.Types 
  ( CsvMap(..)
  , CsvMapped(..)
  , FieldMapping(..)
  , SelectorMapping(..)
  )
import Data.Tapioca.Internal.Decode.Generic
  ( GenericCsvDecode
  , SelectorMeta(..)
  , GParseRecord(..)
  , GParseNamedRecord(..)
  , GSelectorList(..)
  )

import GHC.Generics

import Control.Error.Util
import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.List as L
import qualified Data.Vector as V
import Type.Reflection

-- | Equivalent to cassava's parseRecord
-- Existing of the header determines ordering strategry
-- If header is provided, will treat record as defined in the same order.
-- Otherwise 
parseRecord :: forall r. (CsvMapped r, GenericCsvDecode r) => Maybe (V.Vector B.ByteString) -> C.Record -> C.Parser r
parseRecord mbHeader record = do
   selectorMetas <- eitherParser $ pSelectorMetas @r mbHeader
   parsed <- gParseRecord selectorMetas record
   pure (to parsed)

-- For C.ToRecord
pSelectorMetas :: forall a. (CsvMapped a, GenericCsvDecode a) => Maybe (V.Vector B.ByteString) -> Either String (V.Vector SelectorMeta)
pSelectorMetas hdr = do
  selectorMetasUnordered <- traverse (indexAndMeta hdr) (V.indexed . unCsvMap $ csvMap @a)
  pure $ V.update (snd <$> selectorMetasUnordered) selectorMetasUnordered

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

-- For decoding to C.Record or C.ToNamedRecord. When provided selector mapping determine correct position within csv/header list, and attach metadata necessary to construct from generic
-- If a header is not provided, or element's header is not unique, falls back to mapping back to current position
indexAndMeta :: forall r. GenericCsvDecode r => Maybe (V.Vector B.ByteString) -> (Int, SelectorMapping r) -> Either String (Int, SelectorMeta)
indexAndMeta mbHdr (i, selectorMapping) = do
  let selectors = gSelectorList @(Rep r)
  case selectorMapping of
    fieldHeader := (fm :: FieldMapping r f d e) -> do
      selectorIndex <- L.elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      headerIndex <- case mbHdr of
        Just hdr | V.length (V.filter (== fieldHeader) hdr) == 1
           -> V.elemIndex fieldHeader hdr ?! "Couldn't find header item " <> show fieldHeader <> " in CSV header"
        _ -> pure i
       -- ironically headerIndex is only used for parseRecord
      pure (selectorIndex, Field @f @d typeRep fieldHeader headerIndex (decoder fm))
    Splice (fm :: FieldMapping r f d e) -> do
      selectorIndex <- L.elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      selectorMetas <- pSelectorMetas @d mbHdr
      pure (selectorIndex, Record @f @d typeRep selectorMetas (decoder fm))

eitherParser :: Either String a -> C.Parser a
eitherParser (Left e) = fail e
eitherParser (Right a) = pure a

-- | Equivalent to cassava's parseNamedRecord
parseNamedRecord :: forall r. (CsvMapped r, GenericCsvDecode r) => C.NamedRecord -> C.Parser r
parseNamedRecord namedRecord = do
  selectorMetas <- eitherParser $ pSelectorMetas @r Nothing
  parsed <- gParseNamedRecord selectorMetas namedRecord
  pure (to parsed)
