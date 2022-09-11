primes :: [Int] -> [Int]
primes [] = []
primes (x : xs) = x : primes [y | y <- xs, mod y x /= 0]