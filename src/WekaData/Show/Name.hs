-- |
--
-- Module      :  WekaData.Show.Name
-- License     :  MIT
-- Description :  'Show' instance for 'WekaDataAttribute'.
--
-- Shows only attribute name.


module WekaData.Show.Name where

import WekaData

-- | Shows only attribute name. Defined in "WekaData.Show.Name".
instance Show WekaDataAttribute where show = wekaAttributeName

