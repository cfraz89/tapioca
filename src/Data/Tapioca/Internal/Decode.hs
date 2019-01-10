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
  , SelectorData(..)
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


data ParseRecordData t where
  IndexedRecord :: Maybe C.Header -> ParseRecordData C.Record
  NamedRecord :: ParseRecordData C.NamedRecord

-- | Existence of the header determines ordering strategry
-- If header is provided (as is when parsing from tapioca decode function), will treat record as defined in the same order as the header
-- Without header (as when invoked via cassava FromField instance, we have to assume that the fields are in the same order as the mapping
parseRecord :: forall r t. (CsvMapped r, GenericCsvDecode r t) => ParseRecordData t -> t -> C.Parser r
parseRecord parseData record = do
   selectorMetasUnordered <- traverse (indexedMeta parseData record) (V.indexed . unCsvMap $ csvMap @r)
   let selectorMetas = V.update (snd <$> selectorMetasUnordered) selectorMetasUnordered
   parsed <- gParseRecord selectorMetas record
   pure (to parsed)  

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note
  
-- For decoding to C.Record or C.ToNamedRecord. When provided selector mapping determine correct position within csv/header list, and attach metadata necessary to construct from generic
-- If a header is not provided, or element's header is not unique, falls back to mapping back to current position
indexedMeta :: forall r t. GenericCsvDecode r t => ParseRecordData t -> t -> (Int, SelectorMapping r) -> C.Parser (Int, SelectorData t)
indexedMeta parseData record (i, selectorMapping) = do
  let selectors = gSelectorList @(Rep r)
  case selectorMapping of
    fieldHeader := (fm :: FieldMapping r f d e) -> do
      selectorIndex <- toParser $ L.elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      case parseData of
        IndexedRecord mbHdr -> do
          headerIndex <- case mbHdr of
            Just hdr | V.length (V.filter (== fieldHeader) hdr) == 1
                       -> toParser $ V.elemIndex fieldHeader hdr ?! "Couldn't find header item " <> show fieldHeader <> " in CSV header"
            Nothing -> pure i
          field <- case lookupRecord record headerIndex of
            Just field -> decoder fm <$> C.parseField field
            Nothing -> fail $ "Mapping of field with index " <> show headerIndex <> " not in record"
          pure (selectorIndex, SelectorData field)
    Splice (fm :: FieldMapping r f d e) -> do
      selectorIndex <- toParser $ L.elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      spliceParse <- decoder fm <$> parseRecord parseData record
      pure (0, SelectorData spliceParse)
      --pure (selectorIndex, Record @_ @f @d typeRep selectorMetas (decoder fm))

            
toParser :: Either String a -> C.Parser a
toParser (Left e) = fail e
toParser (Right a) = pure a