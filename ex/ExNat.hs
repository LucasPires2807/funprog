module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show Zero = "O"
    show (Succ m) = "S" ++ show m

instance Eq Nat where

    (==) x y = (x <= y) && (x >= y)

instance Ord Nat where

    (<=) x y = (x < y) || (x == y)

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min (Succ x) (Succ y) = undefined

    max = undefined

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ m) = m

even :: Nat -> Bool
even x = if x <%> 2 == 0 then True else False 

odd :: Nat -> Bool
odd x  = not (even x)

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero     x    = x
(<+>) x        Zero = x
(<+>) (Succ x) y    = Succ((<+>) x y)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) x Zero = x
(<->) Zero y = y
(<->) x y = (<->) (pred x) (pred y)

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) _ Zero = Zero
(<*>) Zero _ = Zero
(<*>) x y = x <+> ((<*>) x (pred y))

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) Zero _ = Zero
(<^>) _ Zero = Succ(Zero)
(<^>) x y = x <*> ((<^>) x (pred y))

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) x y = if x <%> y == 0 
-- (</>) Zero x = Zero
-- (</>) x Zero = error "Quotient cannot be zero"
-- (</>) x (Succ Zero) = x
-- (</>) x y = x <-> ((</>) x (pred y))

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) x y = if x <%> y == 0 then True else False

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero = Succ(Zero)
factorial x = x <*> factorial (pred x)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = Zero
        | x == 0    = Zero
        | otherwise = undefined
