import Data.Ix
import Data.Array
import Haste
import Haste.Graphics.Canvas

bnds = ((0,0), (9,8))
srcTop = (4, 4)
srcBot = (4, 5)

data Tile = Tile { xy :: (Int, Int), path :: [(Int, Int)] } | Blank deriving (Show, Eq)

blankBoard = array bnds [(i, Blank) | i <- range bnds]
dirs = [(1,0),(0,-1),(-1,0),(0,1)]
oppositeDir (x,y) = (-x, -y)

gen [] board _ = board
gen seeds board (r:r1:rs) = let
  n = r `mod` length seeds
  (as, b@(Tile i@(x, y) ds):bs) = splitAt n seeds
  exits = [((x+dx, y+dy), dj) | dj@(dx,dy) <- dirs, let j = (x+dx, y+dy) in inRange bnds j && (board!j) == Blank && (i /= srcTop || dx == 0)]
  in if null exits then gen (as ++ bs) board (r1:rs) else let
    (j, dj) = exits!!(r1 `mod` length exits)
    augTile = Tile i (dj:ds)
    newTile = Tile j [oppositeDir dj]
    in gen ((augTile:newTile:as) ++ bs) (board // [(i, augTile), (j, newTile)]) rs

tileTop = Tile srcTop []
tileBot = Tile srcBot []

main = withElems ["body", "canvas"] $ \[body, canvasElem] -> do
  Just canvas <- getCanvas canvasElem
  seed <- newSeed
  let
    board = gen [tileTop, tileBot] (blankBoard // [(srcTop, tileTop), (srcBot, tileBot)]) $ randomRs (0, 2^20 :: Int) seed
    drawTile Blank = return ()
    drawTile (Tile (x,y) ds) = let (ox,oy) = ((fromIntegral x*32) :: Double, (fromIntegral y*32) :: Double) in sequence_ [ stroke $ line (ox + 16, oy + 16) (ox + 16 + 16 * fromIntegral dx, oy + 16 + 16 * fromIntegral dy) | (dx,dy) <- ds ]
    in do
      render canvas $ sequence_ [drawTile (board!i) | i <- range bnds]

