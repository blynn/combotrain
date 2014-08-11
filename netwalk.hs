import Control.Concurrent.MVar
import Control.Monad
import Data.Ix
import Data.Array
import Data.List
import qualified Data.Map as M
import Haste
import Haste.Graphics.Canvas

bnds = ((0,0), (9,8))
srcTop = (div x 2, div y 2) where (x, y) = snd bnds
srcBot = (x, y + 1) where (x, y) = srcTop
isSrc i = i == srcTop || i == srcBot

data Tile = Tile { xy :: (Int, Int), ways :: [(Int, Int)] } | Blank deriving Eq

data Game = Game { board :: Array (Int, Int) Tile
                 , state :: State
                 , rands :: [Int]
                 , packets :: [((Int, Int), (Int, Int), Int)]
                 }

data State = Won | Play deriving Eq

gen :: [Tile] -> Array (Int, Int) Tile -> [Int] -> (Array (Int, Int) Tile, [Int])
gen [] board (r:rs) = scramble (scrambleSrc board r) rs
gen seeds board (r:r1:rs) = let
  n = r `mod` length seeds
  (as, b@(Tile i@(x, y) w):bs) = splitAt n seeds
  exits = [((x+dx, y+dy), dj) | dj@(dx,dy) <- [(1,0),(0,-1),(-1,0),(0,1)],
   let j = (x+dx, y+dy) in inRange bnds j && (board!j) == Blank && (i /= srcTop || dx == 0)]
  in if null exits then gen (as ++ bs) board (r1:rs) else let
    (j, dj@(dx,dy)) = exits!!(r1 `mod` length exits)
    augTile = Tile i (dj:w)
    newTile = Tile j [(-dx, -dy)]
    in gen ((augTile:newTile:as) ++ bs) (board // [(i, augTile), (j, newTile)]) rs

scramble board rs =
  let f i r | isSrc i   = (i, board!i)
            | otherwise =  (i, iterate rot (board!i) !! (r `mod` 4))
  in (array bnds $ zipWith f (range bnds) rs, drop (rangeSize bnds) rs)

scrambleSrc board r = iterate srcRot board !! (r `mod` 4)

initGame rs = let
  top = Tile srcTop [(0, 1)]
  bot = Tile srcBot [(0, -1)]
  (board, rs1) = gen [top, bot] (array bnds [(i, Blank) | i <- range bnds] // [(srcTop, top), (srcBot, bot)]) rs in Game board Play rs1 []

data Event = KeyDown Int | Click Int Int

rot (Tile i w) = Tile i $ map (\(x, y) -> if y /= 0 then (-y, 0) else (0, x)) w

srcRot board = let
  Tile _ w = rot $ Tile (0, 0) $ delete (0, -1) (ways $ board!srcBot) ++ filter (== (0, -1)) (ways $ board!srcTop)
  in board // [(srcTop, Tile srcTop $ (0, 1) : filter (== (0, -1)) w),
               (srcBot, Tile srcBot $ (0, -1) : filter (/= (0, -1)) w)]

newPackets board i = [(i, dj, 0) | dj <- (ways (board!i))]

handle game@(Game board state _ packets) (Click mx my) = let
  i = (mx `div` 32, my `div` 32)
  in if inRange bnds i then
    if state == Play then
      if isSrc i then game { board = srcRot board }
      else game { board = board // [(i, rot $ board!i)] }
    else
      game { packets = packets ++ newPackets board i }
  else game

handle game (KeyDown 113) = initGame (rands game)
handle game _ = game

lineB :: Int -> Int -> Int -> Int -> Shape ()
lineB x y dx dy = line (0.5 + fromIntegral x, 0.5 + fromIntegral y) (0.5 + fromIntegral (x + dx), 0.5 + fromIntegral (y + dy))

rectB :: Color -> Int -> Int -> Int -> Int -> Picture ()
rectB c x y dx dy = do
  color c $ fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x + dx), fromIntegral (y + dy))
  color (RGB 0 0 0) $ stroke $ rect (fromIntegral x - 0.5, fromIntegral y - 0.5) (fromIntegral (x + dx) + 0.5, fromIntegral (y + dy) + 0.5)

drawB p (x, y) = draw p (fromIntegral x, fromIntegral y)

paint pic = do
  Just can <- createCanvas 32 32
  render can pic
  return can

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
  Just buf <- let (x, y) = snd bnds in createCanvas ((x+1)*32) ((y+1)*32)
  Just grid <- let (x, y) = snd bnds in createCanvas ((x+1)*32) ((y+1)*32)
  Just soln <- let (x, y) = snd bnds in createCanvas ((x+1)*32) ((y+1)*32)
  liveEnd <- paint $ rectB (RGB 255 255 0) 9 9 14 14
  deadEnd <- paint $ rectB (RGB 191 191 191) 10 10 13 13
  packet  <- paint $ (color (RGB 0 0 0) $ fill $ circle (16, 16) 5) >>
                     (color (RGB 255 255 255) $ fill $ circle (16, 16) 4)

  render grid $ color (RGB 192 192 192) $ sequence_
    $ [ stroke $ lineB (x*32) 0 0 288 | x <- [1..9]]
    ++ [ stroke $ lineB 0 (y*32) 320 0 | y <- [1..8]]
  seed <- newSeed
  let
    drawDead Blank = []
    drawDead (Tile (x,y) w) = let (ox,oy) = (x*32, y*32) in [color (RGB 255 127 127) $ stroke $ lineB (ox + 16) (oy + 16) (16 * dx) (16 * dy) | (dx,dy) <- w] ++ if length w == 1 then [drawB deadEnd (ox, oy)] else []

    drawLive i@(x,y) board done = let
      (ox, oy) = (x*32, y*32)
      Tile _ w = board!i
      in foldl (
        \(pics, done) (x, y) -> let (pics1, done1) = drawLive (x, y) board done in (pics ++ pics1, done1)
      ) (
        [color (RGB 0 191 0) $ stroke $ lineB (ox + 16) (oy + 16) (16 * dx) (16 * dy) | (dx,dy) <- w]
        ++ if length w == 1 then [drawB liveEnd (ox, oy)] else [], M.insert (x, y) True done
      ) [(x + dx, y + dy) | (dx,dy) <- w, inRange bnds (x + dx, y + dy), (-dx, -dy) `elem` (ways (board!(x+dx, y+dy))), (x+dx, y+dy) `M.notMember` done]

    loop (Game board state rs packets) = do
      render canvas $ draw buf (0, 0)
      let
        (pics1, done) = drawLive srcBot board M.empty
        pics2 = concat [drawDead (board!i) | i <- range bnds, i `M.notMember` done]
        adv packet@((x, y), (dx, dy), t) =
          if t == 16 - 1 then
            let (x1, y1) = (x + dx, y + dy) in
            [((x1, y1), dj, 0) | dj <- ways $ board!(x1, y1), dj /= (-dx, -dy)]
          else
            [((x, y), (dx, dy), t + 1)]

        packets1 =
          if state == Won && null packets then newPackets board srcBot
          else concat $ map adv packets
        game1 = Game board (if null pics2 then Won else Play) rs packets1
        in do
          if state == Won then do
            render buf $ draw soln (0, 0)
            sequence_ [renderOnTop buf $ drawB packet (32*x + 2*t*dx, 32*y + 2*t*dy) | ((x,y), (dx,dy), t) <- packets]
          else do
            render buf $ draw grid (0, 0)
            renderOnTop buf $ sequence_ $ pics1 ++ pics2
            renderOnTop buf $ let (x,y) = srcTop in rectB (RGB 95 95 191) (x * 32 + 9) (y * 32 + 9) 16 48
            render soln $ draw buf (0, 0)

          q <- swapMVar evq []
          game2 <- return $ if null q then game1 else handle game1 (head q)
          setProp message "innerHTML" $ case state of
            Won -> "Solved"
            _ -> ""
          setTimeout 20 $ loop game2
    in loop $ initGame $ randomRs (0, 2^20 :: Int) seed
