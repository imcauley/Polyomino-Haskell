-- Isaac McAuley
-- Febuary 2018

import Data.List
import Data.Char
import System.Environment (getArgs)


type Block = (Char, Int, Int)
type Piece = [Block]
type Board = [(Piece, (Int, Int))]

piece1 :: Piece
piece1 = [('A', 0, 0), ('A', 0, 1)]
piece2 :: Piece
piece2 = [('B', 0, 0), ('B', 0, 1)]
piece3 :: Piece
piece3 = [('C', 0, 0), ('C', 0, 1)]


piece4 :: Piece
piece4 = [('A', 0, 0),('A', 0, 1),('A', 1, 1)]
piece5 :: Piece
piece5 = [('B', 0, 0)]

main = do
  args <- getArgs
  contents <- readFile (head args)
  let (s:ss) = lines contents
  let board = parseBoard s
  let pieces = parsePieces ss
  let solution = (findSolutions pieces board) !! 0
  let matrix = putBoard solution (createMatrix board)
  (printBoard matrix)

parsePieces :: [String] -> [Piece]
parsePieces [] = []
parsePieces (s:ss) = (parsePiece s (chr  (65 + length ss))): (parsePieces ss)

parsePiece :: String -> Char -> Piece
parsePiece [] _ = []
parsePiece (s1:a:s2:b:rest) c = (c,(digitToInt a), (digitToInt b)) : (parsePiece rest c)

parseBoard :: String -> (Int,Int)
parseBoard (w:s:h:rest) = ((digitToInt w),(digitToInt h))

findSolutions :: [Piece] -> (Int,Int) -> [Board]
findSolutions [] _ = [[]]
findSolutions (p:ps) (w,h) = [(c,(x,y)) : board | board <- (findSolutions ps (w,h)),
                                                  x <- [0..w],
                                                  y <- [0..h],
                                                  c <- (getSet p),
                                                  inBounds c (x,y) (w,h),
                                                  fits c (x,y) board]

fits :: Piece -> (Int, Int) -> Board -> Bool
fits _ _ [] = True
fits p (x,y) ((b,(bx,by)):bs) = noConflicts (offset p (x,y)) (offset b (bx,by)) && (fits p (x,y) bs)

noConflicts :: Piece -> Piece -> Bool
noConflicts [] _ = True
noConflicts ((a, x1, y1):ps1) p2
  | noBlockConflict (a, x1, y1) p2 = noConflicts ps1 p2
  | otherwise = False

noBlockConflict :: Block -> Piece -> Bool
noBlockConflict _ [] = True
noBlockConflict (a, x1, y1) ((b, x2, y2):ps2)
  | x1 == x2 && y1 == y2 = False
  | otherwise = noBlockConflict (a, x1, y1) ps2

inBounds :: Piece -> (Int, Int) -> (Int,Int) -> Bool
inBounds [] _ _ = True
inBounds ((b, bx, by):bs) (x,y) (w,h)
  | (bx + x) >= (w) || (by + y) >= (h) = False
  | (bx + x) < 0 || (by + y) < 0 = False
  | otherwise = inBounds (bs) (x,y) (w,h)

offset :: Piece -> (Int, Int) -> Piece
offset [] _ = []
offset ((b, bx, by):bs) (x,y) = (b, (bx + x), (by + y)) : (offset bs (x,y))

reflect :: Piece -> Int -> Piece
reflect p n = (iterate reflectPiece p) !! n

reflectPiece :: Piece -> Piece
reflectPiece [] = []
reflectPiece ((b, bx, by):bs) = ((b, bx, (-by)): (reflectPiece bs))

rotate :: Piece -> Int -> Piece
rotate p n = (iterate rotatePiece p) !! n

rotatePiece :: Piece -> Piece
rotatePiece [] = []
rotatePiece ((b, bx, by):bs) = ((b, by, (-bx)): (rotatePiece bs))


mins :: Piece -> (Int,Int)
mins [] = (0,0)
mins ((a, ax, ay):as)
  | (ax < (fst (mins as))) && (ay < (snd (mins as))) = (ax,ay)
  | ax < (fst (mins as)) = (ax,(snd (mins as)))
  | ay < (snd (mins as)) = ((fst (mins as)),ay)
  | otherwise = mins as

center :: Piece -> Piece
center [] = []
center p = let off = (mins p)
           in offset p (abs (fst off), abs (snd off))

getSet :: Piece -> [Piece]
getSet p = [center (reflect (rotate p n) q) | n <- [0..3],
                                              q <- [0..1]]

putBoard :: Board -> [[Char]] -> [[Char]]
putBoard [] b = b
putBoard ((a,(x,y)):as) b = putBoard as (putPiece a (x,y) b)

putPiece :: Piece -> (Int,Int) -> [[Char]] -> [[Char]]
putPiece [] _ b = b
putPiece ((a, ax, ay):as) (x,y) b = putPiece as (x,y) (putBlockCol b ((ax + x), (ay + y)) a)

putBlockRow :: [Char] -> Int -> Char -> [Char]
putBlockRow (b:bs) 0 c = c : bs
putBlockRow (b:bs) n c = b : (putBlockRow bs (n-1) c)

putBlockCol :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
putBlockCol (b:bs) (0,m) c = (putBlockRow b m c) : bs
putBlockCol (b:bs) (n,m) c = b : (putBlockCol bs ((n-1), m) c)

createMatrix :: (Int, Int) -> [[Char]]
createMatrix (_, 0) = []
createMatrix (n, m) = (createRow n) : (createMatrix (n, (m-1)))

createRow :: Int -> [Char]
createRow 0 = []
createRow n = '#' : (createRow (n-1))

printBoard :: [String] -> IO ()
printBoard [] = putChar ' '
printBoard a = mapM_ putStrLn a
