data Tree t = NilT | Node t (Tree t) (Tree t)
    deriving (Eq, Show)

depth :: Tree t -> Int
depth NilT = 1
depth (Node parent leftChild rightChild) = 1 + max (depth leftChild) (depth rightChild)

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node parent leftChild rightChild) = [parent] ++ collapse leftChild ++ collapse rightChild

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ NilT = NilT
mapTree f (Node parent leftChild rightChild) = (Node (f parent) (mapTree f leftChild) (mapTree f rightChild) )
