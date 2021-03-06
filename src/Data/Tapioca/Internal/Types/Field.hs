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

module Data.Tapioca.Internal.Types.Field (Field(..), Codec(..), codec, idCodec, encoder, to, (%), decoder) where

import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

import Data.Tapioca.Internal.Types.Capability

data Codec f a = Codec { _encode :: f -> a, _decode :: a -> f }

idCodec :: Codec f f
idCodec = Codec id id

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- c - type to encode and decode as
data Field (s :: Symbol) f c (cs :: [Capability]) r where
  Field :: (r -> f) -> Codec f c -> Field s f c EncodeDecode r
  EncodeField :: (r -> f) -> Field s f f Encode r
  DecodeField :: (c -> f) -> Field s f c Decode r

-- | Perform a bidirectional mapping on this field with the given 'Codec'
codec :: (c -> c') -> (c' -> c) -> Field s f c EncodeDecode r -> Field s f c' EncodeDecode r
codec enc' dec' (Field f (Codec enc dec)) = Field f $ Codec (enc' . enc) (dec . dec')

instance (c~f, HasField x r f, x~x', r~r', f~f') => IsLabel x (Field x' f' c EncodeDecode r') where
  fromLabel = Field (getField @x) idCodec

-- | Perform a mapping of encoder on this EncodeField
encoder :: (f -> f') -> Field s f f Encode r -> Field s f' f' Encode r
encoder fc (EncodeField f) = EncodeField $ fc . f

instance (HasField x r f, x~x', r~r', f~f') => IsLabel x (Field x' f' f' Encode r') where
  fromLabel = EncodeField (getField @x)

-- Arbitrary encoding field
to :: (r -> f) -> Field s f f Encode r
to = EncodeField

infixl 6 %
(%) :: Field s f f Encode r -> Field s' f' f' Encode f -> Field s' f' f' Encode r
(EncodeField l) % (EncodeField r) = EncodeField (r . l)

instance (c~f, HasField x r f, x~x', r~r', f~f') => IsLabel x (Field x' f' c Decode r') where
  fromLabel = DecodeField id

-- | Perform a mapping of decoder on this EncodeField
decoder :: (c' -> c) -> Field s f c Decode r -> Field s f c' Decode r
decoder fc (DecodeField f) = DecodeField $ f . fc