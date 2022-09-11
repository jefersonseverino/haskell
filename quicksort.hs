qs :: [Int] -> [Int]

qs [] = []
qs (x:xs) = qs [a | a <- xs, a < x] ++ [x] ++ qs [a | a <- xs, a >= x]