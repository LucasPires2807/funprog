module ExFromWhile where

fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
fromWhile n f = takeWhile f . drop n

fromFor :: Int -> Int -> [a] -> [a]
fromFor n m = take m . drop n

fromTo :: Int -> Int -> [a] -> [a]
fromTo n m = take (m-1) . drop n

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
fromToThat n m f  = filter f . take (m-1) . drop n
