-- |
--
-- Module      :  WekaData.Show.Full
-- License     :  MIT
-- Description :  'Show' instance for 'WekaDataAttribute'.
--
-- Shows all attribute fields.


module WekaData.Show.Full where

import WekaData

import Data.Set  (elems)
import Data.List (intercalate)


-- | Shows all attribute fields. Defined in "WekaData.Show.Full".
instance Show WekaDataAttribute where show = wekaAttribute2str

-- | Shows /value/: /all attribute fields/. Defined in "WekaData.Show.Full".
instance Show WekaVal where
    show (WVal (attr, val)) = show val ++ ": " ++ show attr

-- | Shows /value/: /all attribute fields/, divided by new lines.
--   Defined in "WekaData.Show.Full".
instance Show WekaEntry where
    show (WEntry set) = intercalate "\n" . map show $ elems set

