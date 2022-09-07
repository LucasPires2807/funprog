halve :: [a] -> ([a],[a])
halve xs = (take mid xs, drop mid xs)
    where
        mid = div (length xs) 2
