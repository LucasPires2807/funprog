module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head [] = error "Can't take an empty list's head."
head (x:_) = x

tail :: [a] -> [a]
tail [] = error "Can't take an empty list's tail."
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length [] = 0
length xs = 1 + length (drop xs)

sum :: Num a => [a] -> a
sum [x] = x
sum (x:xs) = x + (sum xs)

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * (product xs)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = (reverse xs) ++ [x]

(++) :: [a] -> [a] -> [a]
(++) [] []         = []
(++) xs []         = xs
(++) [] ys         = ys
(++) (x:xs) (y:ys) = x:y:(xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
-- maximum :: Ord a => [a] -> a

take :: Integral a => a -> [b] -> [b]
take 0 xs     = []
take n []     = []
take n (x:xs) = x:(take (n-1) xs)

drop :: [a] -> [a]
drop (_:xs) = xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f (x:xs) | f x = x:(takeWhile f xs)
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f (x:xs) | f x       = dropWhile f xs
                   | otherwise = x:xs

-- tails
init :: [a] -> [a]
init xs = take (length xs - 1 ) xs
-- inits

-- subsequences

any :: Eq a => (a -> Bool) -> [a] -> Bool
any f (x:xs) | f x       = True
             | xs == []  = False
             | otherwise = any f xs


all :: Eq a => (a -> Bool) -> [a] -> Bool
all f (x:xs) | not (f x) = False
             | xs == []  = True
             | otherwise = all f xs

-- and :: a -> a -> Bool

-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

zip :: Eq a => Eq b => [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) | ys == []  = []
                  | xs == []  = []
                  | otherwise = (x,y):(zip xs ys)

zipWith :: Eq a => Eq b => (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) | xs == [] = []
                        | ys == [] = []
                        | otherwise = (f x y): zipWith f xs ys

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

