safetail :: [a] -> [a]
-- Conditional expression
--safetail xs = if (length xs == 1) || (length xs == 0) then [] else drop 1 xs

-- Guarded equations
--safetail xs | length xs == 0 = []
--            | length xs == 1 = []
--            | otherwise      = drop 1 xs

-- Pattern matching
--safetail [] = []
--safetail (_:xs) = xs
