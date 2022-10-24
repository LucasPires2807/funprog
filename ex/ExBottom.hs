module ExBottom where

import ExNat
import ExBox

{- For each of the types we have encountered so far
 - (1) how many values does this type have?
 - (2) describe all its values.
 - Create contexts to demonstrate your claims
 - regarding which values are really distinct within
 - each type.
 -}

-- Bool
-- Bool has 3 values:
-- False, True, bottom
-- To show they are distinct with v :: Bool:
-- define:

bottomBool :: Bool
bottomBool = bottomBool

bottomBoolFun :: (Bool, Bool) -> Bool
bottomBoolFun (False, bottomBool) = False
bottomBoolFun (True, _)           = True
bottomBoolFun (_, _)              = True
-- check: bottomBool v

-- Any of (choose one): Int, Char, Double

bottomInt :: Int
bottomInt = bottomInt

bottomIntFun :: Int -> Int -> Int
bottomIntFun x y = x + y

-- Integer

bottomInteger :: Integer
bottomInteger = bottomInteger

bottomIntegerFun :: Integer -> Integer -> Integer
bottomIntegerFun x y = x * y

-- Nat

bottomNat :: Nat
bottomNat = bottomNat

bottomNatFun :: Nat -> Nat -> Nat
bottomNatFun x y = x <+> y

-- Box α

bottomBox :: Box a
bottomBox = bottomBox

bottomBoxFun :: Box a -> Box a -> (a,a)
bottomBoxFun (Box x) (Box y) = (x,y)

-- Tuples: (α,β)

bottomTuple :: (a,b)
bottomTuple = bottomTuple

bottomTupleFun :: (a,b) -> (b,a)
bottomTupleFun (x,y) = (y,x)

-- List α

bottomList :: [a]
bottomList = bottomList

bottomListFun :: [a] -> [a]
bottomListFun = reverse

