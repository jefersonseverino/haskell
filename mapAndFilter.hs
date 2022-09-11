--map and filter

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f xs = foldr (\x xs -> (f x) : xs) [] xs

-- foldr (+) [0] []

filter':: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p xs = foldr (\x xs -> if p x then x:xs else xs ) [] xs
