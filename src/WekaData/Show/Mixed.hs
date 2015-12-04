-- |
--
-- Module      :  WekaData.Show.Mixed
-- License     :  MIT
-- Description :  'Show' instance for 'WekaDataAttribute'.
--
-- Shows full attribute info if calling show on attribute directly,
-- otherwise only attribute names are shown.


module WekaData.Show.Mixed where

import WekaData

import Data.Set  (elems)
import Data.List (intercalate)


-- | Shows all attribute fields. Defined in "WekaData.Show.Mixed".
instance Show WekaDataAttribute where show = wekaAttribute2str

-- | Shows /short attribute name/ -> /value/.
--   Defined in "WekaData.Show.Mixed".
instance Show WekaVal where
    show (WVal attr val) = wekaAttributeName attr ++ "->" ++ val

-- | Shows /short attribute name/ -> /value/ pairs, divided by commas.
--   Defined in "WekaData.Show.Mixed".
instance Show WekaEntry where
    show (WEntry set) = intercalate ", " . map show $ elems set

