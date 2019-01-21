{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Data.Tapioca.Internal.Types.Codec(Codec(..)) where

import GHC.OverloadedLabels
import GHC.Records

import qualified Data.Csv as C
import Data.Profunctor

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- d - Type to decode as
-- e - type to encode as
data Codec r f d e = Codec
  { encoder :: r -> e
  , decoder :: d -> f
  }

instance Profunctor (Codec r f) where
  dimap d e fm = fm
    { encoder = e . encoder fm
    , decoder = decoder fm . d
    }


instance (HasField x r f, f ~ d, f ~ e, C.ToField e, C.FromField d) => IsLabel x (Codec r f d e) where
  fromLabel = Codec (getField @x) id
