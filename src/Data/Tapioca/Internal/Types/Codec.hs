{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Tapioca.Internal.Types.Codec(Codec(..), idCodec) where

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
data Codec (s :: Symbol) r f d e = Codec
  { encoder :: r -> e
  , decoder :: d -> f
  }

instance Profunctor (Codec s r f) where
  dimap d e fm = fm
    { encoder = e . encoder fm
    , decoder = decoder fm . d
    }

idCodec :: forall x r f. HasField x r f => Codec x r f f f
idCodec = Codec (getField @x) id

instance (HasField x r f, f ~ d, f ~ e, C.ToField e, C.FromField d) => IsLabel x (Codec x r f d e) where
  fromLabel = Codec (getField @x) id
