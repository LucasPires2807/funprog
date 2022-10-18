module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

-- define Rat:
data Rat = Rat Integer Integer

instance Show Rat where
    show (Rat a b) = "Rat " ++ show a ++ " " ++ show b

instance Eq Rat where
    (Rat a b) == (Rat x y) = (a*y) == (x*b)

instance Num Rat where
    (Rat a b) + (Rat m n) = Rat (a*n + m*b) (b * n)
    (Rat a b) * (Rat m n) = Rat (a * m) (b * n)
    negate (Rat a b)      = Rat (-a) (b)
    abs (Rat a b)         = Rat (abs(a)) (abs(b))
    signum (Rat a b)      = if (a >= 0 && b > 0) then 1 else -1
    fromInteger x = Rat x 1

instance Ord Rat where
    compare (Rat a b) (Rat x y) | (Rat a b) == (Rat x y) = EQ
                                | a*y < x*b              = LT
                                | otherwise              = GT

rat :: Integer -> Integer -> Rat
rat m n | n == 0 = error "Denominator can't be zero!"
        | otherwise = Rat m n

(//) :: Rat -> Rat -> Rat
(Rat a b) // (Rat m n) = Rat (a * n) (b * m)

denominator :: Rat -> Integer
denominator (Rat m n) = n

numerator :: Rat -> Integer
numerator (Rat m n) = n

