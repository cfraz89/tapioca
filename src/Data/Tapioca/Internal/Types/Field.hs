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

module Data.Tapioca.Internal.Types.Field (Field(..), EncodeField(..), Codec(..), codec, encoder, encodeField) where

import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

data Codec f a = Codec { _encode :: (f -> a), _decode :: (a -> f) }

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- c - type to encode and decode as
data Field (s :: Symbol) r f c = Field
  { _field :: r -> f
  , _codec :: Codec f c
  }

-- | Perform a bidirectional mapping on this field with the given 'Codec'
codec :: Field s r f c -> ((c -> c'), (c' -> c)) -> Field s r f c'
codec (Field f (Codec enc dec)) (enc', dec') = Field f $ Codec (enc' . enc) (dec . dec')

instance (c~f, HasField x r f, x~x', r~r', f~f') => IsLabel x (Field x' r' f' c) where
  fromLabel = Field (getField @x) (Codec id id)

newtype EncodeField r f = EncodeField (r -> f)

encodeField :: (r -> f) -> EncodeField r f
encodeField = EncodeField

-- | Perform a mapping of encoder on this EncodeField
encoder :: EncodeField r f -> (f -> c) -> EncodeField r c
encoder (EncodeField f) fc = EncodeField $ fc . f

instance (HasField x r f, r~r', f~f') => IsLabel x (EncodeField r' f') where
  fromLabel = EncodeField (getField @x)