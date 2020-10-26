{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

--TODO upstream to 'evdev' - note that the Generic instance takes an annoyingly long time to derive

module Orphans where

import Evdev.Codes (Key (..))
import Options.Generic (Generic, ParseField, ParseFields, ParseRecord)

deriving instance Generic Key
instance ParseField Key
instance ParseRecord Key
instance ParseFields Key
