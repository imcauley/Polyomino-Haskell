-- Isaac McAuley
-- Febuary 2018

import Data.List
import Data.Char
import System.Environment (getArgs)


type Block = (Char, Int, Int)
type Piece = [Block]
type Board = [(Piece, (Int, Int))]


main = do
  args <- getArgs
  contents <- readFile (head args)
  let (s:ss) = lines contents
  let board = parseBoard s
  let pieces = parsePieces ss
  case ((findSolutions pieces board) !! 0) of
    (Just n) -> printBoard (putBoard n (createMatrix board))
    Nothing -> putStrLn "No solution"




-- INPUT

parsePieces :: [String] -> [Piece]
parsePieces [] = []
parsePieces (s:ss) = (parsePiece s (chr  (65 + length ss))): (parsePieces ss)

parsePiece :: String -> Char -> Piece
parsePiece [] _ = []
parsePiece (_:a:_:b:rest) c = (c,(digitToInt a), (digitToInt b)) : (parsePiece rest c)

parseBoard :: String -> (Int,Int)
parseBoard (w:_:h:rest) = ((digitToInt w),(digitToInt h))



-- SOLUTION FINDER

findSolutions :: [Piece] -> (Int,Int) -> [Maybe Board]
findSolutions [] _ = [Just []]
findSolutions (p:ps) (w,h) = [Just ((c,(x,y)) : board) | Just board <- (findSolutions ps (w,h)),
                                                         x <- [0..w],
                                                          y <- [0..h],
                                                          c <- (getSet p),
                                                          inBounds c (x,y) (w,h),
                                                          fits c (x,y) board] ++ [Nothing]

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



-- PIECE GENERATION

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

removeDupes :: [Piece] -> [Piece]
removeDupes [] = []
removeDupes (p:ps) = p : ((removeDupePiece p ps) ++ (removeDupes (removeDupePiece p ps)))

removeDupePiece :: Piece -> [Piece] -> [Piece]
removeDupePiece p [] = []
removeDupePiece a (b:ps)
  | samePiece a b = removeDupePiece a ps
  | otherwise = b : (removeDupePiece a ps)

samePiece :: Piece -> Piece -> Bool
samePiece [] [] = True
samePiece _ [] = False
samePiece [] _ = False
samePiece ((a, ax, ay):as) ((b, bx, by):bs)
  | (ax == bx) && (ay == by) = samePiece as bs
  | (ax == by) && (ay == bx) = samePiece as bs
  | otherwise = False

getSet :: Piece -> [Piece]
getSet p = removeDupes [center (reflect (rotate p n) q) | n <- [0..3],
                                                          q <- [0..1]]



-- OUTPUT

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
putBlockCol (b:bs) (n,0) c = (putBlockRow b n c) : bs
putBlockCol (b:bs) (n,m) c = b : (putBlockCol bs (n, (m-1)) c)

createMatrix :: (Int, Int) -> [[Char]]
createMatrix (_, 0) = []
createMatrix (n, m) = (createRow n) : (createMatrix (n, (m-1)))

createRow :: Int -> [Char]
createRow 0 = []
createRow n = '#' : (createRow (n-1))

printBoard :: [String] -> IO ()
printBoard [] = putChar ' '
printBoard a = mapM_ putStrLn a
