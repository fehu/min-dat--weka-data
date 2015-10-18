-- |
--
-- Module      :  WekaData.Show.Full
-- License     :  MIT
-- Description :  'Show' instance for 'WekaDataAttribute'.
--
-- Shows all attribute fields.


module WekaData.Show.Full where

import WekaData

-- | Shows all attribute fields. Defined in "WekaData.Show.Full".
instance Show WekaDataAttribute where show = wekaAttribute2str

