{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Encode
  ( toRecord
  , toNamedRecord
  , header
  ) where

import Data.Tapioca.Internal.Types.Mapping

import Control.Monad.Reader
import qualified Data.Csv as C
import qualified Data.Vector as V

-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- | Similar to cassava's headerOrder function
header :: forall r. CsvMapped r => C.Header
header = fromCsvMap (csvMap @r)
  where fromCsvMap (CsvMap (m :: t)) = hFoldMap @_ @C.Header id  m
    
-- | Tapioca equivalent of cassava's toRecord
toRecord :: forall r. CsvMapped r => r -> C.Record
toRecord record = foldCsvMap (csvMap @r)
  where foldCsvMap (CsvMap (m :: t)) = hFoldMap @_ @(Reader r (V.Vector C.Field)) (`runReader` record) m

-- | Tapioca equivalent of cassava's toNamedRecord
toNamedRecord :: CsvMapped r => r -> C.NamedRecord
toNamedRecord = undefined
-- toNamedRecord record = foldMap namedRecord (unCsvMap csvMap)
--     where namedRecord (name := fm) = HM.singleton name (C.toField $ encoder fm record)
--           namedRecord (Splice fm) = toNamedRecord $ encoder fm record
