--                Width Height Pieces
import Data.List

data Block = Block Char Int Int deriving (Show)
data Piece = Piece [Block] deriving (Show)
data Board = Board [(Piece, Int, Int)] deriving (Show)
data PieceCollection = PC [Piece]

data Solution = Solution (Piece, Int, Int) deriving (Show)


piece1 = Piece [(Block 'A' 0 0), (Block 'A' 0 1), (Block 'A' 1 1)]
piece2 = Piece [(Block 'B' 0 0), (Block 'B' 0 1), (Block 'B' 1 1)]
piece3 = Piece [(Block 'C' 0 0), (Block 'C' 0 1), (Block 'C' 1 1)]



solutions :: [Solution]
solutions = board1 [piece1, piece2, piece3]

findSolutions :: [Piece] -> Int -> Int -> [Board]
findSolutions [] _ _ = [(Board [])]
{-
findSolutions (p:ps) w h  = [(p, x, y) : board | board <- (findSolutions ps w h),
                                                 x <- [0..w],
                                                 y <- [0..h],
                                                 fits p x y board]
-}
fits :: Piece -> Int -> Int -> Board -> Bool
fits (Piece ((Block a ax ay):as)) x y (Board (b:bs)) = True


{-
rotatePiece :: Piece -> Piece
rotatePiece (Piece x) = (Piece (rotateBlocks x))

rotateBlocks :: [Block] -> [Block]
rotateBlocks [] = []
rotateBlocks ((Block a x y):xs) = (Block a y (-x)):(rotateBlocks xs)


blockThere :: Board -> (Int,Int) -> Bool
blockThere (Board _ _ []) _ = False
blockThere (Board w h ((Block _ x1 y1):bs)) (x2, y2)
  | x1 == x2 && y1 == y2 = True
  | otherwise = blockThere (Board w h bs) (x2,y2)


toList :: Board -> [(Int,Int)]
toList (Board w h []) = []
toList (Board w h ((Block a x y):xs)) = (x,y) : (toList (Board w h (xs) ))


isNotInBoard :: Block -> Board -> Bool
isNotInBoard (Block a x y) (Board w h []) = True
isNotInBoard (Block a x y) (Board w h ((Block b bx by):xs))
  | x == bx && y == by = False
  | otherwise = isNotInBoard (Block a x y) (Board w h xs)

inBounds :: Block -> Board -> Bool
inBounds (Block a x y) (Board w h _) = x >= 0 && x < w && y >= 0 && y < h

placeBlock :: Block -> Board -> Maybe Board
placeBlock (Block a x y) (Board w h n)
  | not (inBounds (Block a x y) (Board w h n)) = Nothing
  | not (isNotInBoard (Block a x y) (Board w h n)) = Nothing
  | otherwise = Just (Board w h ((Block a x y):n))

placePiece :: Piece -> Board -> Maybe Board
placePiece (Piece []) (Board w h n) = Just (Board w h n)
placePiece (Piece (b:bs)) (Board w h n) =
  let nb = (placeBlock b (Board w h n)) in
    if isNothingBoard nb then
      Nothing
    else placePiece (Piece (bs)) (justBoard nb)

isNothingBoard :: Maybe Board -> Bool
isNothingBoard Nothing = True
isNothingBoard (Just b) = False

justBoard :: Maybe Board -> Board
justBoard (Just b) = b
-}
