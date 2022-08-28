inity :: [a] -> [a]
inity xs = reverse (drop 1 (reverse xs))
