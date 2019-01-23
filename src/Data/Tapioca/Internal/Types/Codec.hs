{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Types.Codec(Codec(..), SCodec(..), trimCodec, idCodec) where

import GHC.OverloadedLabels
import GHC.Records

import qualified Data.Csv as C
import Data.Profunctor
import GHC.TypeLits

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- d - Type to decode as
-- e - type to encode as
data SCodec (s :: Symbol) r f d e = SCodec
  { sEncoder :: f -> e
  , sDecoder :: d -> f
  }

data Codec r f d e = Codec
  { encoder :: f -> e
  , decoder :: d -> f
  }

instance Profunctor (SCodec s r f) where
  dimap d e fm = fm
    { sEncoder = e . sEncoder fm
    , sDecoder = sDecoder fm . d
    }

trimCodec :: SCodec s r f d e -> Codec r f d e
trimCodec (SCodec enc dec) = Codec enc dec

idCodec :: (f~d, f~e) => Codec r f d e
idCodec = Codec id id

instance (f~d, f~e, HasField x r f, x~x', r~r', f~f') => IsLabel x (SCodec x' r' f' d e) where
  fromLabel = SCodec id id
