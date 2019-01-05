{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Functions needed in both encoding and decoding
module Data.Tapioca.Internal.Common (header) where

import Data.Tapioca.Internal.Types

import qualified Data.ByteString as B
import qualified Data.Vector as V

-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- | Similar to cassava's headerOrder function
header :: forall r. CsvMapped r => V.Vector B.ByteString
header = foldMap names $ unCsvMap (csvMap @r)
  where names (name := _) = pure name
        names (Splice (_ :: FieldMapping r f d e)) = header @f