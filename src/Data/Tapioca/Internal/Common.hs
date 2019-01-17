{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

-- | Functions needed in both encoding and decoding
module Data.Tapioca.Internal.Common (header, (?!), toParser) where

import GHC.Records

import Data.Tapioca.Internal.Types

import qualified Data.Csv as C
import Control.Error.Util
import qualified Data.ByteString as B
import qualified Data.Vector as V

-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- | Similar to cassava's headerOrder function
header :: forall r. CsvMapped r => V.Vector B.ByteString
header = foldMap names $ unCsvMap (csvMap @r)
  where names (name := _) = pure name
        names (Splice (_ :: Codec x i r f d e)) = header @f

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

toParser :: Either String a -> C.Parser a
toParser (Left e) = fail e
toParser (Right a) = pure a