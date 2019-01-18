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
  ( CsvMapped(..)
  , FieldMapping(..)
  , Codec(..)
  , GenericCsvDecode
  , GParseRecord(..)
  )

import Type.Reflection
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
parseRecord :: forall r t. CsvMapped r => ParseRecord t -> t -> C.Parser r
parseRecord = undefined
-- parseRecord parseData record = do
--    selectorDataMapOrder <- traverse (indexedMeta parseData record) (V.indexed . unCsvMap $ csvMap @r)
--    let selectorData = V.update (snd <$> selectorDataMapOrder) selectorDataMapOrder
--    parsed <- gParseRecord proxy# selectorData
--    pure (to parsed)

-- For decoding to C.Record or C.ToNamedRecord. When provided selector mapping determine correct position within csv/header list, and attach metadata necessary to construct from generic
-- If a header is not provided, or element's header is not unique, falls back to mapping back to current position
indexedMeta :: forall r t. CsvMapped r => ParseRecord t -> t -> (Int, Int) -> C.Parser (Int, Int)
indexedMeta = undefined
-- indexedMeta pr record (i, selectorMapping) = case selectorMapping of
--     name := (fm :: FieldMapping x i r f d e) ->
--       case pr of
--         Record mbHdr -> do
--           headerIndex <- case mbHdr of
--             Just hdr | V.length (V.filter (== name) hdr) == 1
--                        -> toParser $ V.elemIndex name hdr ?! "Couldn't find header item " <> show name <> " in CSV header"
--             _ -> pure i
--           field <- C.parseField (record V.! headerIndex)
--           indexed fm field
--         NamedRecord -> do
--           field <- C.parseField =<< toParser (HM.lookup name record ?! "Field name " <> show name <> " not in record")
--           indexed fm field
--     Splice (fm :: FieldMapping x i r f d e) -> indexed fm =<< case pr of
--       Record _ -> parseRecord pr record
--       NamedRecord -> parseRecord pr record

-- indexed :: forall x i r f d e. (Typeable f, GenericCsvDecode r) => FieldMapping x i r f d e -> d -> C.Parser (Int, SelectorData)
-- indexed fm d = do
--   selectorIndex <- toParser $ L.elemIndex "" (gSelectorList @(Rep r)) ?! "Record type doesn't have selector " <> ""
--   pure (selectorIndex, SelectorData (decoder fm d))

  -- TODO Need to be handing raw position in case Splice -> Record no header