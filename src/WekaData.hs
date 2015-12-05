{-|

Module      : WekaData
Description : Works with weka *.arff data and files.
License     : MIT
Stability   : development

Works with weka *.arff data and files.

-}

module WekaData (

-- * Raw Data
  RawWekaData(..)

-- * Attributes
, WekaDataAttribute(WekaAttrNum, WekaAttrNom)
, wekaAttributeName
, wekaAttribute2str

-- * Search by attr. name
, findInMap
, findInMapWithAttr
, lookupInMap
, lookupInMapWithAttr
, lookupInSet
, lookupInList


-- * *.arff files
, readWekaData

, wekaDataFromLines

-- * Data Containers
, WekaVal (WVal)
, WekaEntry(..)
, wekaData2Sparse

-- * Search 'WekaVal' by attr. name
, lookupWValInMap
, lookupWValInSet

) where

import Data.Typeable
import Data.Function (on)
import Data.List
import Data.List.Split
import Data.Char
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import Control.Applicative

-----------------------------------------------------------------------------

-- | Data read from Weka @*.arff@ files.
data RawWekaData = RawWekaData { rwdName      :: String              -- ^ relation
                               , rwdAttrs     :: [WekaDataAttribute] -- ^ attributes
                               , rawWekaData  :: [[String]]          -- ^ raw data
                               }

instance (Show WekaDataAttribute) =>
    Show RawWekaData where
        show (RawWekaData name attrs dta) =
            "RawWekaData " ++ name ++ "\n" ++
            "Attributes:\n\t" ++ intercalate "\n\t" (map wekaAttribute2str attrs) ++
            "Data:\n" ++ intercalate "\n\t" (map show dta)

-- | Weka attribute.
data WekaDataAttribute = WekaAttrNum String          -- ^ numeric attribute
                       | WekaAttrNom String [String] -- ^ nominal attribute with its domain
                       | WekaAttrExtractor String    -- ^ an extractor intented to be used for sets and maps
                    deriving Typeable

instance Eq WekaDataAttribute where
    x == y = wekaAttributeName x == wekaAttributeName y

instance Ord WekaDataAttribute where
    compare = compare `on` wekaAttributeName

getFromMaybe name = fromMaybe (error $ "attribute '" ++ name ++ "' not found")

-- | Find an attribute by name in a map with attribute keys.
findInMap :: String -> Map WekaDataAttribute a -> a
findInMap name = getFromMaybe name . lookupInMap name
-- fromMaybe (error $ "attribute '" ++ name ++ "' not found") . lookupInMap name

-- | Try to find an attribute by name in a map with attribute keys.
lookupInMap :: String -> Map WekaDataAttribute a -> Maybe a
lookupInMap name = Map.lookup (WekaAttrExtractor name)

-- | Find an attribute by name in a map with attribute keys.
findInMapWithAttr :: String -> Map WekaDataAttribute a -> (WekaDataAttribute, a)
findInMapWithAttr name = getFromMaybe name . lookupInMapWithAttr name


lookupElem :: (Int -> a -> b) -> (WekaDataAttribute -> a -> Maybe Int) -> String -> a -> Maybe b
lookupElem elemAt lookupIndex name x =
    fmap (`elemAt` x) (lookupIndex (WekaAttrExtractor name) x)


-- | Try to find an attribute by name in a map with attribute keys.
lookupInMapWithAttr :: String -> Map WekaDataAttribute a -> Maybe (WekaDataAttribute, a)
lookupInMapWithAttr = lookupElem Map.elemAt Map.lookupIndex

-- | Try to find an attribute by name in a set of attributes.
lookupInSet :: String -> Set WekaDataAttribute -> Maybe WekaDataAttribute
lookupInSet = lookupElem Set.elemAt Set.lookupIndex

lookupInList :: String -> [WekaDataAttribute] -> Maybe WekaDataAttribute
lookupInList = lookupElem (flip (!!)) elemIndex


-- | Get name of a 'WekaDataAttribute'.
wekaAttributeName (WekaAttrNum name)       = name
wekaAttributeName (WekaAttrNom name _)     = name
wekaAttributeName (WekaAttrExtractor name) = name

wekaAttribute2str (WekaAttrNum name)        = "Numeric " ++ name
wekaAttribute2str (WekaAttrNom name domain) = "Nominal " ++ name ++ " " ++ show domain
wekaAttribute2str (WekaAttrExtractor name)  = "Extractor " ++ name

-----------------------------------------------------------------------------

data WekaVal = WVal WekaDataAttribute String
             | WValExtractor String
             deriving Typeable

instance Eq WekaVal where
    (WVal a1 v1) == (WVal a2 v2)        = a1 == a2 && v1 == v2
    (WVal a v) == (WValExtractor n)     = wekaAttributeName a == n
    e@(WValExtractor _) == v@(WVal _ _) = v == e

instance Ord WekaVal where
    (WVal a1 v1) `compare` (WVal a2 v2)        | a1 == a2  = v1 `compare` v2
                                               | otherwise = a1 `compare` a2
    (WVal a _) `compare` (WValExtractor n)     = wekaAttributeName a `compare` n
    e@(WValExtractor n) `compare` v@(WVal a _) = n `compare` wekaAttributeName a

lookupWVal elemAt lookupIndex name x =
    fmap (`elemAt` x) (lookupIndex (WValExtractor name) x)


-- | Try to find a 'WekaVal' by attribute name in a set of 'WekaVal's.
lookupWValInMap :: String -> Map WekaVal v -> Maybe WekaVal
lookupWValInMap = lookupWVal (\x -> fst . Map.elemAt x) Map.lookupIndex

-- | Try to find a 'WekaVal' by attribute name in a set of 'WekaVal's.
lookupWValInSet :: String -> Set WekaVal -> Maybe WekaVal
lookupWValInSet = lookupWVal Set.elemAt Set.lookupIndex

newtype WekaEntry = WEntry (Set WekaVal) deriving (Eq, Ord, Typeable)

-----------------------------------------------------------------------------
-- | Tries to read a *.arff file.
readWekaData :: String          -- ^ file name
             -> IO RawWekaData
readWekaData filename = do lines <- splitOn "\n" <$> readFile filename
                                 -- same as fmap (splitOn "\n") (readFile filename)
                           return $ wekaDataFromLines lines

wekaDataFromLines :: [String] -> RawWekaData
wekaDataFromLines lines = readWekaData' lines Nothing [] []

readWekaData' :: [String] -> Maybe String -> [WekaDataAttribute] -> [[String]] -> RawWekaData

-- ignore comments and empty lines
readWekaData' (l:lines) name attrs dta
    | "%" `isPrefixOf` l || null l = readWekaData' lines name attrs dta

-- handling name
readWekaData' (l:lines) Nothing [] []
    | "@relation " `isPrefixOf` l =
        let name = dropComment $ drop (length "@relation ") l
        in readWekaData' lines (Just name) [] []

-- handling attributes
readWekaData' (l:lines) name@(Just _) attrs []
    | "@attribute " `isPrefixOf` l =
        let attr = readWekaAttr . dropSpaces . dropComment $ l
        in readWekaData' lines name (attr:attrs) []

-- handling data
readWekaData' (l:lines) name@(Just _) attrs@(_:_) dta
    | "@data" `isPrefixOf` l = readWekaData' lines name attrs dta
    | otherwise              = readWekaData' lines name attrs (splitOn "," l : dta)

-- return result
readWekaData' [] (Just name) attrs dta = RawWekaData name (reverse attrs) (reverse dta)

dropComment = takeWhile (/= '%')
dropSpaces = dropWhile isSpace

-- | Reads weka attribute from a line.
readWekaAttr :: String -> WekaDataAttribute
readWekaAttr line | null l'                   = error $ "readWekaAttr: empty attribute: " ++ show line
                  | head l' == '{'            = WekaAttrNom name domain
                  | "numeric" `isPrefixOf` l' = WekaAttrNum name
                  | otherwise = error $ show l'
    where l    = dropSpaces $ drop (length "@attribute ") line
          (name, len) = if head l == '\'' then (takeWhile (/= '\'') (drop 1 l), 2)
                                          else (takeWhile (/= ' ') l, 0)
          l'     = dropSpaces $ drop (length name + len) l
          f      = filter (fmap not $ (||) <$> isSpace <*> (`elem` "{}"))
          domain = map f $ splitOn "," l'


-----------------------------------------------------------------------------
-- | in the data: __foreach__ nominal attribute with /singleton/ domain:
--
--        1. drop the '?' items
--        2. create a 'WekaEntry' for the rest of attributes
wekaData2Sparse :: RawWekaData -> [WekaEntry]
wekaData2Sparse (RawWekaData  _ attrs dta) = do
    entry <- dta
    return . WEntry . Set.fromList . map (uncurry WVal)
           . filter ((/=) "?" . snd) $ zip attrs entry





