import Control.Concurrent.MVar
import Control.Monad
import Data.Ix
import Data.Array
import qualified Data.Map as M
import Haste
import Haste.Graphics.Canvas

bnds = ((0,0), (9,8))
srcTop = (4, 4)
srcBot = (4, 5)
isSrc i = i == srcTop || i == srcBot

data Tile = Tile { xy :: (Int, Int), ways :: [(Int, Int)] } | Blank deriving (Show, Eq)

blankBoard = array bnds [(i, Blank) | i <- range bnds]
dirs = [(1,0),(0,-1),(-1,0),(0,1)]

gen :: [Tile] -> Array (Int, Int) Tile -> [Int] -> Array (Int, Int) Tile
gen [] board rs = scramble board rs
gen seeds board (r:r1:rs) = let
  n = r `mod` length seeds
  (as, b@(Tile i@(x, y) ds):bs) = splitAt n seeds
  exits = [((x+dx, y+dy), dj) | dj@(dx,dy) <- dirs, let j = (x+dx, y+dy) in inRange bnds j && (board!j) == Blank && (i /= srcTop || dx == 0)]
  in if null exits then gen (as ++ bs) board (r1:rs) else let
    (j, dj@(dx,dy)) = exits!!(r1 `mod` length exits)
    augTile = Tile i (dj:ds)
    newTile = Tile j [(-dx, -dy)]
    in gen ((augTile:newTile:as) ++ bs) (board // [(i, augTile), (j, newTile)]) rs

scramble board rs =
  let f i r | isSrc i   = (i, board!i)
            | otherwise =  (i, iterate rot (board!i) !! (r `mod` 4))
  in array bnds $ zipWith f (range bnds) rs

tileTop = Tile srcTop []
tileBot = Tile srcBot []

data Event = KeyDown Int | Click Int Int

rot (Tile i ds) = Tile i $ map (\(x, y) -> if y /= 0 then (-y, 0) else (0, x)) ds

handle board (Click mx my) = let
  i = (mx `div` 32, my `div` 32)
  in if isSrc i then board else board // [(i, rot $ board!i)]
handle board _ = board

lineB :: Int -> Int -> Int -> Int -> Shape ()
lineB x y dx dy = line (0.5 + fromIntegral x, 0.5 + fromIntegral y) (0.5 + fromIntegral (x + dx), 0.5 + fromIntegral (y + dy))

rectB :: Int -> Int -> Int -> Int -> Picture ()
rectB x y dx dy = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x + dx), fromIntegral (y + dy))

rectC :: Int -> Int -> Int -> Int -> Picture ()
rectC x y dx dy = stroke $ rect (fromIntegral x - 0.5, fromIntegral y - 0.5) (fromIntegral (x + dx) + 0.5, fromIntegral (y + dy) + 0.5)

main = withElems ["body", "canvas", "message"] $ \[body, canvasElem, message] -> do
  evq <- newEmptyMVar
  putMVar evq []
  canvasElem  `onEvent` OnMouseDown $ \_button (x, y) -> do
    q <- takeMVar evq
    putMVar evq (q ++ [Click x y])
  body `onEvent` OnKeyDown $ \_k -> do
    q <- takeMVar evq
    putMVar evq (q ++ [KeyDown _k])
  Just canvas <- getCanvas canvasElem
  seed <- newSeed
  let
    initBoard = gen [tileTop, tileBot] (blankBoard // [(srcTop, tileTop), (srcBot, tileBot)]) $ randomRs (0, 2^20 :: Int) seed
    drawDead Blank = []
    drawDead (Tile (x,y) ds) = let (ox,oy) = (x*32, y*32) in [ color (RGB 255 127 127) $ stroke $ lineB (ox + 16) (oy + 16) (16 * dx) (16 * dy) | (dx,dy) <- ds ] ++ if length ds == 1 then [color (RGB 191 191 191) $ rectB (ox+10) (oy+10) 13 13] else []

    drawLive i@(x,y) board done = let
      (ox, oy) = (x*32, y*32)
      Tile _ ds = board!i
      in foldl (
        \(pics, done) (x, y) -> let (pics1, done1) = drawLive (x, y) board done in (pics ++ pics1, done1)
      ) ([color (RGB 0 191 0) $ stroke $ lineB (ox + 16) (oy + 16) (16 * dx) (16 * dy) | (dx,dy) <- ds] ++ if length ds == 1 then [color (RGB 255 255 0) $ rectB (ox+9) (oy+9) 14 14, color (RGB 0 0 0) $ rectC (ox+9) (oy+9) 14 14] else [], M.insert (x, y) True done) [(x + dx, y + dy) | (dx,dy) <- ds, inRange bnds (x + dx, y + dy), (-dx, -dy) `elem` (ways (board!(x+dx, y+dy))), (x+dx, y+dy) `M.notMember` done]

    loop board = do
      render canvas $ color (RGB 192 192 192) $ sequence_
        $ [ stroke $ lineB (x*32) 0 0 288 | x <- [1..9]]
        ++ [ stroke $ lineB 0 (y*32) 320 0 | y <- [1..8]]

      let
        (pics, done) = drawLive srcTop board M.empty
        (pics1, done1) = drawLive srcBot board done
        pics2 = concat [drawDead (board!i) | i <- range bnds, i `M.notMember` done1]
        isWon = null pics2
        in do
          renderOnTop canvas $ sequence_ $ pics ++ pics1
          renderOnTop canvas $ sequence_ pics2
          renderOnTop canvas $ let (x,y) = srcTop in do
            color (RGB 0 0 255) $ rectB (x * 32 + 9) (y * 32 + 9) 16 48
            color (RGB 0 0 0) $ rectC (x * 32 + 9) (y * 32 + 9) 16 48
          q <- swapMVar evq []
          when isWon $ setProp message "innerHTML" "Well done!"
          setTimeout 32 $ loop ( if null q then board else handle board (head q) )
    in loop initBoard
