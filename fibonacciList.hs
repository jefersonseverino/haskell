fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = (fib (n - 1) + fib (n - 2))

paresFib :: Int -> [Int]
paresFib n = gerarLista n 1 0

gerarLista :: Int -> Int -> Int -> [Int]

gerarLista n nFib qtd
    | (n == qtd) = []
    | mod (fib (nFib)) 2 == 0 = [fib (nFib)] ++ gerarLista n (nFib + 1) (qtd + 1)
    | otherwise = gerarLista n (nFib + 1) qtd