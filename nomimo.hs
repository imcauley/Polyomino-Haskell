import Data.List

type Block = (Char, Int, Int)
type Piece = [Block]
type Board = [(Piece, (Int, Int))]

piece1 :: Piece
piece1 = [('A', 0, 0), ('A', 0, 1)]
piece2 :: Piece
piece2 = [('B', 0, 0), ('B', 0, 1)]
piece3 :: Piece
piece3 = [('C', 0, 0), ('C', 0, 1)]
coord1 :: (Int, Int)
coord1 = (2,3)

piece4 :: Piece
piece4 = [('A', 0, 0),('A', 0, 1),('A', 1, 1)]
piece5 :: Piece
piece5 = [('B', 0, 0)]

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

reflect :: Piece -> Piece
reflect [] = []
reflect ((b, bx, by):bs) = ((b, bx, (-by)): (rotate bs))

rotate :: Piece -> Piece
rotate [] = []
rotate ((b, bx, by):bs) = ((b, by, (-bx)): (rotate bs))

maxX :: Piece -> Int
maxX [] = 0
maxX ((b, bx, by):bs)
  | bx >= (maxX bs) = bx
  | otherwise = maxX bs

maxY :: Piece -> Int
maxY [] = 0
maxY ((b, bx, by):bs)
  | by >= (maxX bs) = by
  | otherwise = maxX bs


getSet :: Piece -> [Piece]
getSet p = [p,
            (offset (rotate p) (0,(maxX p))),
            (offset (rotate (rotate p)) (0,(2 * (maxX p)))),
            (offset (rotate (rotate (rotate p))) (0,(3 * (maxX p)))),
            (offset (reflect p) ((maxY p),0)),
            (offset (rotate (reflect p)) ((maxY p),(maxX p))),
            (offset (rotate (rotate (reflect p))) ((maxY p),(2 * (maxX p)))),
            (offset (rotate (rotate (rotate (reflect p)))) ((maxY p),(3 * (maxX p))))]