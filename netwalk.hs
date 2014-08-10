import System.Random
import Data.Ix
import Data.Array

bnds = ((0,0), (10,9))
srcTop = (5, 5)
srcBot = (5, 6)

data Tile = Tile { xy :: (Int, Int), path :: [(Int, Int)] } | Blank deriving (Show, Eq)

blankBoard = array bnds [(i, Blank) | i <- range bnds]
dirs = [(1,0),(0,1),(-1,0),(0,-1)]
oppositeDir (x,y) = (-x, -y)

gen [] board _ = board
gen seeds board (r:r1:rs) = let
  n = r `mod` length seeds
  (as, b@(Tile i@(x, y) path):bs) = splitAt n seeds
  exits = [((x+dx, y+dy), dj) | dj@(dx,dy) <- dirs, let j = (x+dx, y+dy) in inRange bnds j && (board!j) == Blank]
  in if null exits then gen (as ++ bs) board (r1:rs) else let
    (j, dj) = exits!!(r1 `mod` length exits)
    newTile = Tile j [oppositeDir dj]
    in gen (newTile:seeds) (board // [(i, Tile i (dj:path)), (j, newTile)]) rs

tileTop = Tile srcTop []
tileBot = Tile srcBot []

main = do
  g <- getStdGen
  putStrLn . show $ gen [tileTop, tileBot] (blankBoard // [(srcTop, tileTop), (srcBot, tileBot)]) $ randoms g
