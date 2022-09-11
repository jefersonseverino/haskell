type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

coordX :: Ponto -> Float
coordX (a, b) = a

coordY :: Ponto -> Float
coordY (a, b) = b

isVertical :: Reta -> Bool
isVertical ((a, b), (c, d))
    | (a == c) = True
    | otherwise = False

pontoY :: Float -> Reta -> Float

pontoY x ( (x1, y1), (x2, y2) )
    | isVertical ((x1, y1), (x2, y2)) = 0
    | otherwise = ((((x-x1)*(y2-y1))/(x2 - x1)) + y1)

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [
    ("Sergio","O Senhor dos Aneis"), 
    ("Andre","Duna"),
    ("Fernando","Jonathan Strange & Mr. Norrell"),
    ("Fernando","A Game of Thrones")
    ]

membro :: [Int] -> Int -> Bool
membro lista v 
    | (length [n | n <- lista, n == v]) == 1 = True
    | otherwise = False

livros :: BancoDados -> Pessoa -> [Livro]
livros bd pessoa = [livro | (pess, livro) <- bd, pessoa == pess]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd livro = [pessoa | (pessoa, book) <- bd, livro == book]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd livro
    | (length [ book | (pessoa, book) <- bd, livro == book]) == 1 = True
    | otherwise = False

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd pessoa = length ( [livro | (pess, livro) <- bd, pess == pessoa] )

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd pessoa livro = [(pess, book) | (pess, book) <- bd, not (pessoa == pess && livro == book) ]

qs :: [Int] -> [Int]
qs [] = []
qs (x : xs) = qs([n | n <- (x : xs), (n < x)]) ++ [x] ++ qs([n | n <- (x : xs), (n > x)])

getWord :: String -> String
getWord [] = []
getWord (x : xs)
    | (x /= ' ') = x:getWord xs
    | otherwise = []

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
    | (x /= ' ') = dropWord xs
    | otherwise = (x:xs)

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x : xs)
    | (x == ' ') = dropSpace xs
    | otherwise = (x:xs)

type Wordd = String

splitWords :: String -> [Wordd]
splitWords letras = [str | str <- (getWord letras) : (dropSpace ( dropWord(letras) ) ) ]