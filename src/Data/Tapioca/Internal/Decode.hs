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
  ( ParseRecord(..)
  , parseRecord
  ) where 

import Data.Tapioca.Internal.Common ((?!), toParser)
import Data.Tapioca.Internal.Types 
  ( CsvMap(..)
  , CsvMapped(..)
  , FieldMapping(..)
  , SelectorMapping(..)
  )
import Data.Tapioca.Internal.Decode.Generic
  ( GenericCsvDecode
  , SelectorData(..)
  , GParseRecord(..)
  , GSelectorList(..)
  )

import GHC.Generics

import qualified Data.Csv as C
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

data ParseRecord t where
  Record :: Maybe C.Header -> ParseRecord C.Record
  NamedRecord :: ParseRecord C.NamedRecord

-- | Existence of the header determines ordering strategry
-- If header is provided (as is when parsing from tapioca decode function), will treat record as defined in the same order as the header
-- Without header (as when invoked via cassava FromField instance, we have to assume that the fields are in the same order as the mapping
parseRecord :: forall r t. (CsvMapped r, GenericCsvDecode r t) => ParseRecord t -> t -> C.Parser r
parseRecord parseData record = do
   selectorMetasUnordered <- traverse (indexedMeta parseData record) (V.indexed . unCsvMap $ csvMap @r)
   let selectorMetas = V.update (snd <$> selectorMetasUnordered) selectorMetasUnordered
   parsed <- gParseRecord selectorMetas record
   pure (to parsed)  

  
-- For decoding to C.Record or C.ToNamedRecord. When provided selector mapping determine correct position within csv/header list, and attach metadata necessary to construct from generic
-- If a header is not provided, or element's header is not unique, falls back to mapping back to current position
indexedMeta :: forall r t. GenericCsvDecode r t => ParseRecord t -> t -> (Int, SelectorMapping r) -> C.Parser (Int, SelectorData t)
indexedMeta parseData record (i, selectorMapping) = case selectorMapping of
    name := (fm :: FieldMapping r f d e) ->
      case parseData of
        Record mbHdr -> do
          headerIndex <- case mbHdr of
            Just hdr | V.length (V.filter (== name) hdr) == 1
                       -> toParser $ V.elemIndex name hdr ?! "Couldn't find header item " <> show name <> " in CSV header"
            _ -> pure i
          field <- C.parseField (record V.! headerIndex)
          indexed fm $ SelectorData (decoder fm field)
        NamedRecord -> do
          field <- C.parseField =<< toParser (HM.lookup name record ?! "Field name " <> show name <> " not in record")
          indexed fm $ SelectorData (decoder fm field)
    Splice (fm :: FieldMapping r f d e) -> do
      let pr = case parseData of
            Record _ -> parseRecord
            NamedRecord -> parseRecord
      spliceParse <- decoder fm <$> pr parseData record
      pure (0, SelectorData spliceParse)
    where  indexed :: FieldMapping r f d e -> SelectorData t -> C.Parser (Int, SelectorData t)
           indexed fm sd = do
            selectorIndex <- toParser $ L.elemIndex (selector fm) (gSelectorList @(Rep r)) ?! "Record type doesn't have selector " <> selector fm
            pure (selectorIndex, sd)