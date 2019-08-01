{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Data.Tapioca.Internal.Types.Field (Field(..), Codec(..), codec, idCodec, encoder, by, (>.)) where

import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

import Data.Tapioca.Internal.Types.CsvMapType

data Codec f a = Codec { _encode :: f -> a, _decode :: a -> f }

idCodec :: Codec f f
idCodec = Codec id id

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- c - type to encode and decode as
data Field (s :: Symbol) f c (t :: CsvMapType) r where
  Field :: (r -> f) -> Codec f c -> Field s f c 'Both r
  EncodeField :: (r -> f) -> Field s f f 'Encode r

-- | Perform a bidirectional mapping on this field with the given 'Codec'
codec :: (c -> c') -> (c' -> c) -> Field s f c 'Both r -> Field s f c' 'Both r
codec enc' dec' (Field f (Codec enc dec)) = Field f $ Codec (enc' . enc) (dec . dec')

instance (c~f, HasField x r f, x~x', r~r', f~f') => IsLabel x (Field x' f' c 'Both r') where
  fromLabel = Field (getField @x) idCodec


-- | Perform a mapping of encoder on this EncodeField
encoder :: (f -> f') -> Field s f f 'Encode r -> Field s f' f' 'Encode r
encoder fc (EncodeField f) = EncodeField $ fc . f

instance (HasField x r f, x~x', r~r', f~f') => IsLabel x (Field x' f' f' 'Encode r') where
  fromLabel = EncodeField (getField @x)

-- Arbitrary encoding field
by :: (r -> f) -> Field s f f 'Encode r
by = EncodeField

infixl 6 >.
(>.) :: Field s f f 'Encode r -> Field s' f' f' 'Encode f -> Field s' f' f' 'Encode r
(EncodeField l) >. (EncodeField r) = EncodeField (r . l)
