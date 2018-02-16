{- We want to solve the 8 queens problem.
   This is a special purpose program.
   Not the n-queens problem. Just 8.

   This seems like a 2-d array problem. But,
   we don't really need a 2-d array.

   We can work with co-ordinates of where the
   8 queens will go.
-}

type Solution = [(Int, Int)]

solutions :: [Solution] -- so if we just want one we can do solution !! 0
solutions = placeCols [1..8]

placeCols :: [Int] -> [Solution]
placeCols [] = [[]]
placeCols (x:xs) = [(x,y) : sol | sol <- placeCols xs,
                                  y <- [1..8],
                                  noAttacks x y sol ]

noAttacks :: Int -> Int -> Solution -> Bool
noAttacks x y = and . map noAttack
    where noAttack (x',y') = (y /= y') && (abs (x - x') /= abs (y - y'))



-- Here's a fun (and actually useful) built in type.
-- I'm going to make this type using mispellings

data Maibe a = Noothing | Joost a

-- Example of how to do this.

divWithErrorSignal :: Int -> Int -> Maybe Int
divWithErrorSignal n d | (d /= 0)  = Just (n `div` d)
                       | otherwise = Nothing
{-
*Main> divWithErrorSignal 5 2
Just 2
*Main> divWithErrorSignal 5 0
Nothing
-}

-- Using "show"

data Tree a = Node a [Tree a] -- | EmptyTree

instance Show a => Show (Tree a) where
   show (Node value xs) = "Node (" ++ (show value) ++ " " ++ (show xs) ++ ")"

exampleTree = Node 5 [Node 1 [], Node 2 [], Node 3 [Node 4 []], Node 5 []]

-- Another one

data BinTree a = BNode a (BinTree a) (BinTree a) | EmptyBinTree

instance Show a => Show (BinTree a) where
   show (BNode value left right) = "(BNode " ++ (show value) ++
                                   " Left " ++ (show left) ++
                                   " Right " ++ (show right) ++ " )"
   show EmptyBinTree = "EmptyBinTree"

exampleBinTree = BNode 5 (BNode 3 EmptyBinTree EmptyBinTree)
                         (BNode 7 EmptyBinTree EmptyBinTree)

binTreeMap :: (a -> b) -> BinTree a -> BinTree b
binTreeMap _ EmptyBinTree = EmptyBinTree
binTreeMap f (BNode v left right) = BNode (f v) (binTreeMap f left)
                                                (binTreeMap f right)
