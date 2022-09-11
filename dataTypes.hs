data Estacao = Inverno | Verao | Outono | Primavera deriving (Show, Eq)
data Temp = Frio | Quente deriving (Show, Eq)

clima :: Estacao -> Temp
clima Inverno = Frio
clima _ = Quente

type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade
    deriving (Show)

showPerson :: Pessoas -> String
showPerson (Pessoa n a) = n ++ " -- " ++ show a

data Shape = Circle Float | Rectangle Float Float | Square Float
    deriving (Show)

area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle a b) = a * b
area (Square l) = l*l

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound _ = False

