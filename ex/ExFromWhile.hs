module ExFromWhile where

fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
fromWhile n f = takeWhile f . drop (n-2)

fromFor :: Int -> Int -> [a] -> [a]
fromFor n m = drop (n-1) . take m

fromTo :: Int -> Int -> [a] -> [a]
fromTo = fromFor

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
fromToThat n m f  = takeWhile f . drop (n-1) . take m
