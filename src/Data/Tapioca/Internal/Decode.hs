{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Decode
  ( decode
  ) where 

import Data.Tapioca.Types 
  ( Header(..)
  , CsvMap(..)
  , CsvMapped(..)
  , FieldMapping(..)
  , SelectorMapping(..)
  )
import Data.Tapioca.Internal.Decode.Generic
  ( GenericCsvDecode
  , SelectorMeta(..)
  , GParseRecord(..)
  , GSelectorList(..)
  )

import GHC.Generics

import Control.Error.Util
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.Csv.Parser as CP
import Data.List
import qualified Data.Vector as V
import Type.Reflection

-- Parse the required data from the csv file
csvParser :: forall a. (CsvMapped a, GenericCsvDecode a) => Header -> Parser (Maybe (V.Vector B.ByteString), C.Csv)
csvParser useHeader = do
  hdr <- case useHeader of
    WithHeader -> Just <$> (CP.header . fromIntegral . fromEnum) ','
    WithoutHeader -> pure Nothing
  records <- CP.csv C.defaultDecodeOptions
  pure (hdr, records)


-- Used for decoding
pSelectorMetas :: forall a. (CsvMapped a, GenericCsvDecode a) => Maybe (V.Vector B.ByteString) -> Either String (V.Vector SelectorMeta)
pSelectorMetas hdr = do
  selectorMetasUnordered <- traverse (positionOf hdr) (V.indexed . unCsvMap $ csvMap @a)
  pure $ V.update (snd <$> selectorMetasUnordered) selectorMetasUnordered

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

-- For decoding. With selector mapping determine correct position within csv/header list, and attach metadata necessary to construct from generic
positionOf :: forall r. GenericCsvDecode r => Maybe (V.Vector B.ByteString) -> (Int, SelectorMapping r) -> Either String (Int, SelectorMeta)
positionOf mbHdr (i, selectorMapping) = do
  let selectors = gSelectorList @(Rep r)
  case selectorMapping of
    fieldHeader := (fm :: FieldMapping r f e d) -> do
      selectorIndex <- elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      headerIndex <- case mbHdr of
        Just hdr -> V.elemIndex fieldHeader hdr ?! "Couldn't find header item " <> show fieldHeader <> " in CSV header"
        Nothing -> pure i
      pure (selectorIndex, Field @f @d typeRep headerIndex (decoder fm))
    Splice (fm :: FieldMapping r f e d) -> do
      selectorIndex <- elemIndex (selector fm) selectors ?! "Record type doesn't have selector " <> selector fm
      selectorMetas <- pSelectorMetas @d mbHdr
      pure (selectorIndex, Record @f @d typeRep selectorMetas (decoder fm))

-- | Decode a CSV String. If there is an error parsion, error message is returned on the left
decode :: forall a. (CsvMapped a, GenericCsvDecode a) => Header -> B.ByteString -> Either String (V.Vector a)
decode useHeader bs = do
   (mbHdr, csv) <- parseOnly (csvParser @a useHeader) bs
   selectorMetas <- pSelectorMetas @a mbHdr
   C.runParser $ traverse ((to <$>) . gParseRecord selectorMetas) csv