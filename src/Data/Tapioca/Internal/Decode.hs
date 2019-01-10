{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Tapioca.Internal.Decode
  ( parseRecord
  , eitherParser
  ) where 

import Data.Tapioca.Internal.Types 
  ( CsvMap(..)
  , CsvMapped(..)
  , FieldMapping(..)
  , SelectorMapping(..)
  )
import Data.Tapioca.Internal.ReifyRecord
import Data.Tapioca.Internal.Decode.Generic
  ( GenericCsvDecode
  , SelectorMeta(..)
  , GParseRecord(..)
  , GSelectorList(..)
  )

import GHC.Generics

import Control.Applicative
import Control.Error.Util
import qualified Data.Csv as C
import qualified Data.List as L
import qualified Data.Vector as V
import Type.Reflection

-- | Existence of the header determines ordering strategry
-- If header is provided (as is when parsing from tapioca decode function), will treat record as defined in the same order as the header
-- Without header (as when invoked via cassava FromField instance, we have to assume that the fields are in the same order as the mapping
parseRecord :: forall r t. (CsvMapped r, GenericCsvDecode r C.Record) => Maybe C.Header -> C.Record -> C.Parser r
parseRecord mbHeader record = do
   selectorMetas <- getSelectorMetas @r (indexedMetaRecord mbHeader)
   parsed <- gParseRecord selectorMetas record
   pure (to parsed)

parseNamedRecord :: forall r t. (CsvMapped r, GenericCsvDecode r C.NamedRecord) => C.NamedRecord -> C.Parser r
parseNamedRecord record = do
   selectorMetas <- getSelectorMetas @r indexedMetaNamedRecord
   parsed <- gParseRecord selectorMetas record
   pure (to parsed)

getSelectorMetas :: forall r t. (CsvMapped r, GenericCsvDecode r t, ReifyRecord t, Typeable t) => ((Int, SelectorMapping r) -> C.Parser (Int, SelectorMeta t)) -> C.Parser (V.Vector (SelectorMeta t))
getSelectorMetas getMeta = do
  selectorMetasUnordered <- traverse getMeta (V.indexed . unCsvMap $ csvMap @r)
  pure $ V.update (snd <$> selectorMetasUnordered) selectorMetasUnordered

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

-- For decoding to C.Record or C.ToNamedRecord. When provided selector mapping determine correct position within csv/header list, and attach metadata necessary to construct from generic
-- If a header is not provided, or element's header is not unique, falls bacqk to mapping back to current position
indexedMetaRecord :: forall r. GenericCsvDecode r C.Record => Maybe C.Header -> (Int, SelectorMapping r) -> C.Parser (Int, SelectorMeta C.Record)
indexedMetaRecord mbHdr (i, selectorMapping) = do
  let selectors = gSelectorList @(Rep r)
  case selectorMapping of
    fieldHeader := (fm :: FieldMapping r f d e) -> toParser $ do
      selectorIndex <- L.elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      headerIndex <- case mbHdr of
        Just hdr | V.length (V.filter (== fieldHeader) hdr) == 1
           -> V.elemIndex fieldHeader hdr ?! "Couldn't find header item " <> show fieldHeader <> " in CSV header"
        _ -> pure i
      pure (selectorIndex, Field @_ @f @d typeRep headerIndex (decoder fm))
    Splice (fm :: FieldMapping r f d e) -> do
      selectorIndex <- toParser $ L.elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      selectorMetas <- getSelectorMetas @d (indexedMetaRecord mbHdr)
      pure $ (selectorIndex, Record @_ @f @d typeRep selectorMetas (decoder fm))

            
toParser :: Either String a -> C.Parser a
toParser (Left e) = fail e
toParser (Right a) = pure a