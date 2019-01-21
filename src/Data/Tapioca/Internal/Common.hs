{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}

-- | Functions needed in both encoding and decoding
module Data.Tapioca.Internal.Common (header, (?!), toParser, parseWithCsvMap) where

import Data.Tapioca.Internal.Types

import GHC.Exts
import GHC.Generics

import Control.Error.Util
import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.Vector as V

-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- | Similar to cassava's headerOrder function
header :: forall r. CsvMapped r => V.Vector B.ByteString
header = undefined
-- header = fromCsvMap (csvMap @r)
--  where fromCsvMap (CsvMap t) = foldOn t
--        foldOn t@(x :| xs) = hFoldMap allNames t
--        allNames (name := _) = pure name
--        allNames (Splice (_ :: Codec r f d e)) = header @f

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

toParser :: Either String a -> C.Parser a
toParser (Left e) = fail e
toParser (Right a) = pure a

parseWithCsvMap :: forall r t. CsvMapped r => C.NamedRecord -> C.Parser r
parseWithCsvMap nr = parseFrom (csvMap @r)
    where parseFrom :: CsvMap r -> C.Parser r
          parseFrom (CsvMap (m :: m)) = to <$> gParseRecord @(Rep r) @r @m  proxy# m nr
