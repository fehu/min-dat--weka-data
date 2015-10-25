-- |
--
-- Module      :  WekaData.Show.Name
-- License     :  MIT
-- Description :  'Show' instance for 'WekaDataAttribute'.
--
-- Shows only attribute name.


module WekaData.Show.Name where

import WekaData

import Data.Set  (elems)
import Data.List (intercalate)


-- | Shows only attribute name. Defined in "WekaData.Show.Name".
instance Show WekaDataAttribute where show = wekaAttributeName

-- | Shows /short attribute name/ -> /value/.
--   Defined in "WekaData.Show.Name".
instance Show WekaVal where
    show (WVal (attr, val)) = show attr ++ " -> " ++ show val

-- | Shows /short attribute name/ -> /value/ pairs, divided by commas.
--   Defined in "WekaData.Show.Name".
instance Show WekaEntry where
    show (WEntry set) = intercalate ", " . map show $ elems set

