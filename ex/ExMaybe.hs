module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes []     = []
catMaybes (x:xs) | isJust x  = (fromJust x) : (catMaybes xs)
                 | otherwise = catMaybes xs

fromJust :: Maybe a -> a
fromJust (Just a) = a

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust _      = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

listToMaybe :: [a] -> Maybe a
listToMaybe []   = Nothing
listToMaybe (x:_) = Just x

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f []     = []
mapMaybe f (x:xs) | isJust (f x)  = (fromJust $ f x) : mapMaybe f xs
                  | otherwise = mapMaybe f xs

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x f m | isNothing m = x
            | otherwise   = f (fromJust m)

maybeToList :: Maybe a -> [a]
maybeToList m | isNothing m = []
              | otherwise   = [fromJust m]

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith []     _       = []
tryToModifyWith _      []      = []
tryToModifyWith (m:ms) (x:xs)  | isJust m = (fromJust m) x : tryToModifyWith ms xs
                               | otherwise = tryToModifyWith ms xs
