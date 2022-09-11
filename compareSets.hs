module Set where

data Set t = Set [t]
    deriving (Show)

instance Eq t => Eq (Set t) where
    (Set []) == (Set []) = True
    (Set (a:as)) == (Set (b:bs)) = eq_Set (a:as) (b:bs)
    _ == _ = False

eq_Set :: Eq t => [t] -> [t] -> Bool
eq_Set [] _ = True
eq_Set (x:xs) (y:ys)
    | isIn x (y:ys) = eq_Set xs (y:ys)
    | otherwise = False

isIn :: Eq t => t -> [t] -> Bool
isIn n [] = False
isIn n (x:xs)
    | (x == n) = True
    | otherwise = isIn n xs