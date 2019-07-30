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

module Data.Tapioca.Internal.Types.Field (Field(..), EncodeField(..), Codec(..), codec, idCodec, encoder, encodeConst) where

import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

data Codec f a = Codec { _encode :: f -> a, _decode :: a -> f }

idCodec :: Codec f f
idCodec = Codec id id

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- c - type to encode and decode as
data Field (s :: Symbol) f c r = Field
  { _field :: r -> f
  , _codec :: Codec f c
  }

-- | Perform a bidirectional mapping on this field with the given 'Codec'
codec :: (c -> c') -> (c' -> c) -> Field s f c r -> Field s f c' r
codec enc' dec' (Field f (Codec enc dec)) = Field f $ Codec (enc' . enc) (dec . dec')

instance (c~f, HasField x r f, x~x', r~r', f~f') => IsLabel x (Field x' f' c r') where
  fromLabel = Field (getField @x) idCodec

newtype EncodeField (s :: Symbol) f r = EncodeField (r -> f)

-- | Perform a mapping of encoder on this EncodeField
encoder :: (f -> c) -> EncodeField s f r -> EncodeField s c r
encoder fc (EncodeField f) = EncodeField $ fc . f

instance (HasField x r f, x~x', r~r', f~f') => IsLabel x (EncodeField x' f' r') where
  fromLabel = EncodeField (getField @x)

encodeConst :: f -> EncodeField "" f r
encodeConst = EncodeField . const
