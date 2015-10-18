{-|

Module      : WekaData
Description : Works with weka *.arff data and files.
License     : MIT
Stability   : development

Works with weka *.arff data and files.

-}

module WekaData (

  RawWekaData(..)
, WekaDataAttribute(WekaAttrNum, WekaAttrNom)

, wekaAttributeName
, wekaAttribute2str

, findInMap
, findInMapWithAttr
, lookupInMap
, lookupInMapWithAttr
, lookupInSet

, readWekaData

, wekaData2Sparse

) where

import Data.Typeable
import Data.Function (on)
import Data.List
import Data.List.Split
import Data.Char
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


-- | Get name of a 'WekaDataAttribute'.
wekaAttributeName (WekaAttrNum name)       = name
wekaAttributeName (WekaAttrNom name _)     = name
wekaAttributeName (WekaAttrExtractor name) = name

wekaAttribute2str (WekaAttrNum name)        = "Numeric " ++ name
wekaAttribute2str (WekaAttrNom name domain) = "Nominal " ++ name ++ " " ++ show domain
wekaAttribute2str (WekaAttrExtractor name)  = "Extractor " ++ name

-----------------------------------------------------------------------------
-- | Tries to read a *.arff file.
readWekaData :: String          -- ^ file name
             -> IO RawWekaData
readWekaData filename = do lines <- splitOn "\n" <$> readFile filename
                                 -- same as fmap (splitOn "\n") (readFile filename)
                           return $ readWekaData' lines Nothing [] []

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
--        1. replace the domain value by attribute name
--        2. drop the '?' items
wekaData2Sparse :: RawWekaData -> [[String]]
wekaData2Sparse (RawWekaData  _ attrs dta) =
    do its <- dta
       return $ do (it, i) <- zip its [0..]
                   case Map.lookup i sd of Just name -> case it of "?" -> []
                                                                   _   -> [name]
                                           _         -> [it]
    where singletonDomains = do (WekaAttrNom name [_], i) <- zip attrs [0..]
                                return (i, name)
          sd   = Map.fromList singletonDomains









