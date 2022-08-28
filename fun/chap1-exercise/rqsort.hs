rqsort[] = []
rqsort(x:xs) = rqsort larger ++ [x] ++ rqsort smaller
    where
        larger = [a | a <- xs, a > x]
        smaller = [b | b <- xs, b <= x]
