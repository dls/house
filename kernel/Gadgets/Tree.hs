module Tree where

-- First the def of Tree.

data Tree a 
	= Node Int a (Tree a) (Tree a)
	| Leaf Int a
	deriving (Show)


leaf :: Int -> Tree ()
leaf x = Leaf x () 

initTree :: Tree ()
initTree = Node 9 () (leaf 4) (Node 6 () (leaf 7) (leaf 3))

sumTree :: Tree a -> Int
sumTree (Node a _ t1 t2) = a + sumTree t1 + sumTree t2
sumTree (Leaf a _)       = a 


newTree :: [Int] -> Tree () -> Tree ()
newTree [] (Leaf x _) =
 	Node top () (leaf left) (leaf right)
    where
	(top,left,right) = splitNumber x
newTree []    tr@(Node x _ t1 t2) = Leaf (sumTree tr) ()
newTree (1:adds) (Node a b t1 t2) = Node a b (newTree adds t1) t2
newTree (2:adds) (Node a b t1 t2) = Node a b t1 (newTree adds t2)
newTree _        _		  = error "newTree error"

change :: (a -> a) -> Int -> [a] -> [a]
change fn 1 (x:xs) = fn x : xs
change fn n (x:xs) = x : change fn (n-1) xs
change fn _ _      = error "change failed"
  
splitNumber :: Int -> (Int,Int,Int)
splitNumber x = (x - x3 * 2,x3,x3)
   where
	x3 = x `div` 3

addAddr :: [Int] -> Tree () -> Tree [Int]
addAddr addr (Node x () t1 t2)
	= Node x addr (addAddr (addr++[1]) t1) (addAddr (addr++[2]) t2)
addAddr addr (Leaf x ())
	= Leaf x addr 

treeDepth :: Tree a -> Int
treeDepth (Leaf _ _) = 1
treeDepth (Node _ _ t1 t2) = 1 + max (treeDepth t1) (treeDepth t2)

pot :: Int -> Int
pot x = iterate (*2) 1 !! x

addCoords :: Tree a -> Tree (a,(Int,Int))
addCoords tree = addCoords' (p `div` 2) h (p,0) tree
  where
	p = pot h
	h = treeDepth tree


addCoords' :: Int -> Int -> (Int,Int) -> Tree d -> Tree (d,(Int,Int))
addCoords' d yd (x,y) (Leaf i a) = Leaf i (a,(x,y))
addCoords' d yd (x,y) (Node i a t1 t2) =
	Node i (a,(x,y))
	(addCoords' (d `div` 2) (yd-1) (x-d,y+yd) t1)
	(addCoords' (d `div` 2) (yd-1) (x+d,y+yd) t2)

