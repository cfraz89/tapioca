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
  , parseRecord'
  , eitherParser
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
  , GSelectorList(..)
  , ReifyRecord(..)
  )

import GHC.Generics

import Control.Applicative
import Control.Error.Util
import qualified Data.Csv as C
import qualified Data.List as L
import qualified Data.Vector as V
import Type.Reflection

-- | Existence of the header determines ordering strategry
-- If header is provided, will treat record as defined in the same order.
-- This is used when actually parsing a full csv file
-- Without header, we have to assume that the fields are in the same order as the mapping
parseRecord' :: forall r t. (CsvMapped r, GenericCsvDecode r t, ReifyRecord t, Typeable t) => Maybe C.Header -> t -> C.Parser r
parseRecord' mbHeader record = do
   selectorMetas <- eitherParser $ pSelectorMetas @r mbHeader
   parsed <- gParseRecord selectorMetas record
   pure (to parsed)

-- | Equivalent to cassava's parseRecord and parseNamedRecord. Just raw row decoding.
parseRecord :: forall r t. (CsvMapped r, GenericCsvDecode r t, ReifyRecord t, Typeable t) => t -> C.Parser r
parseRecord = parseRecord' Nothing

pSelectorMetas :: forall a t. (CsvMapped a, GenericCsvDecode a t, ReifyRecord t, Typeable t) => Maybe C.Header -> Either String (V.Vector (SelectorMeta t))
pSelectorMetas hdr = do
  selectorMetasUnordered <- traverse (indexAndMeta hdr) (V.indexed . unCsvMap $ csvMap @a)
  pure $ V.update (snd <$> selectorMetasUnordered) selectorMetasUnordered

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

-- For decoding to C.Record or C.ToNamedRecord. When provided selector mapping determine correct position within csv/header list, and attach metadata necessary to construct from generic
-- If a header is not provided, or element's header is not unique, falls bacqk to mapping back to current position
indexAndMeta :: forall r t. (GenericCsvDecode r t, ReifyRecord t, Typeable t) => Maybe C.Header -> (Int, SelectorMapping r) -> Either String (Int, SelectorMeta t)
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
      pure (selectorIndex, Field @_ @f @d typeRep (fieldIndex @t fieldHeader headerIndex) (decoder fm))
    Splice (fm :: FieldMapping r f d e) -> do
      selectorIndex <- L.elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      record <- as @C.Record  <|> as @C.NamedRecord
      pure (selectorIndex,  record)
      where as :: forall tr. (Typeable tr, GenericCsvDecode d tr) => Either String (SelectorMeta t)
            as = case eqTypeRep (typeRep @t) (typeRep @tr) of
              Just HRefl -> do
                selectorMetas <- pSelectorMetas @d @t mbHdr
                pure $ Record @_ @f @d typeRep selectorMetas (decoder fm)
              Nothing -> fail $ "Wrong type " <> show (typeRep @t)
            
eitherParser :: Either String a -> C.Parser a
eitherParser (Left e) = fail e
eitherParser (Right a) = pure a