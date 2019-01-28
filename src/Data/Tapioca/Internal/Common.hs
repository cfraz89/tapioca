{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | Functions needed in both encoding and decoding
module Data.Tapioca.Internal.Common
  ( (?!)
  , toParser
  , bsVectorString
  , DecodeIndexing(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Error.Util
import qualified Data.Csv as C

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

toParser :: Either String a -> C.Parser a
toParser (Left e) = fail e
toParser (Right a) = pure a

bsVectorString :: [B.ByteString] -> String
bsVectorString = BC.unpack . BC.intercalate ","

data DecodeIndexing r t where
  DecodeNamed :: DecodeIndexing r C.NamedRecord -- assumes presence of header
  DecodeOrdered :: C.HasHeader -> DecodeIndexing r C.Record

