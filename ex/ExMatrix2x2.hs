module ExMatrix2x2
    ( matrix
    , zero
    , identity
    , rows
    , cols
    , getElem
    , transpose
    , det
    , isDiagonal
    , isTriangular
    , isLowerTriangular
    , isUpperTriangular
    , singular
    , invertible
    , inverse
    ) where

type Number = Double
type Row = [Number]
type Col = [Number]

data Matrix2x2 = Matrix2x2 Number Number Number Number

instance Show Matrix2x2 where
    show (Matrix2x2 a b c d) = "Row Col " ++ (show a) ++ " Col " ++ (show b) ++ " Row Col " ++ (show c) ++ " Col " ++ (show d)

instance Eq Matrix2x2 where
    (Matrix2x2 a b c d) == (Matrix2x2 w x y z)  | a == w && b == x && c == y && d == z = True
                                                | otherwise = False

instance Num Matrix2x2 where
    (Matrix2x2 a b c d) + (Matrix2x2 w x y z) = Matrix2x2 (a+w) (b+x) (c+y) (d+z)
    (Matrix2x2 a b c d) * (Matrix2x2 w x y z) = Matrix2x2 (a*w+b*x) (a*x+b*z) (c*w+d*y) (c*x+d*z)
    negate (Matrix2x2 a b c d) = (Matrix2x2 (-a) (-b) (-c) (-d))
    abs (Matrix2x2 a b c d) = (Matrix2x2 (abs a) (abs b)  (abs c) (abs d))
    signum (Matrix2x2 a b c d) | (a < 0) || (b < 0) || (c < 0) || (d < 0) = -1
                               | otherwise                                = 1
    fromInteger a = (Matrix2x2 (fromInteger a)  (fromInteger a)  (fromInteger a)  (fromInteger a))

-- matrix a b c d should create the matrix
-- ( a c )
-- ( b d )
matrix :: Number -> Number -> Number -> Number -> Matrix2x2
matrix a b c d = Matrix2x2 a b c d

zero :: Matrix2x2
zero = Matrix2x2 0.0 0.0 0.0 0.0

identity :: Matrix2x2
identity = Matrix2x2 1.0 0.0 0.0 1.0

rows :: Matrix2x2 -> [Row]
rows (Matrix2x2 a b c d) = [[a,c],[b,d]]

cols :: Matrix2x2 -> [Col]
cols (Matrix2x2 a b c d) = [[a,b],[c,d]]

getElem :: (Int,Int) -> Matrix2x2 -> Number
getElem (x,y) (Matrix2x2 a b c d) | x == 1 && y == 1 = a
                                  | x == 2 && y == 1 = b
                                  | x == 1 && y == 2 = c
                                  | x == 2 && y == 2 = d
                                  | otherwise = error "Matrix2x2 do not have the given entry element"

transpose :: Matrix2x2 -> Matrix2x2
transpose (Matrix2x2 a b c d) = Matrix2x2 a b c d

det :: Matrix2x2 -> Number
det (Matrix2x2 a b c d) = (a*d) - (b*c)

isDiagonal :: Matrix2x2 -> Bool
isDiagonal m@(Matrix2x2 a b c d) = m == identity

isTriangular :: Matrix2x2 -> Bool
isTriangular m@(Matrix2x2 a b c d) | (isLowerTriangular m && isUpperTriangular m == False) || (isUpperTriangular m && isLowerTriangular m == False) = True
                                   | otherwise = False
-- isTriangular = undefined

isLowerTriangular :: Matrix2x2 -> Bool
isLowerTriangular (Matrix2x2 a b c d) | c == 0    = True
                                      | otherwise = False

isUpperTriangular :: Matrix2x2 -> Bool
isUpperTriangular (Matrix2x2 a b c d) | b == 0    = True
                                      | otherwise = False

singular :: Matrix2x2 -> Bool
singular m@(Matrix2x2 a b c d) = (det m) == 0

invertible :: Matrix2x2 -> Bool
invertible = not . singular

inverse :: Matrix2x2 -> Matrix2x2
inverse m@(Matrix2x2 a b c d) | invertible m = Matrix2x2 (((1 / det m)) * d) (((1/ det m)) * (-c)) (((1/ det m)) *(-b)) (((1 / det m)) * a)
                              | otherwise    = error "This matrix does not have inverse"

