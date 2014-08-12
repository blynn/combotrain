import Control.Concurrent.MVar
import Control.Monad
import Data.Ix
import Data.Array
import Data.List
import Haste
import Haste.Graphics.Canvas

bnds = ((0,0), (9,8))
srcTop = (div x 2, div y 2) where (x, y) = snd bnds
srcBot = (x, y + 1) where (x, y) = srcTop
isSrc i = i == srcTop || i == srcBot

data Tile = Tile { xy :: (Int, Int), ways :: [(Int, Int)] } | Blank deriving Eq

data Game = Game { board   :: Array (Int, Int) Tile
                 , live    :: Array (Int, Int) Bool
                 , state   :: State
                 , rands   :: [Int]
                 , packets :: [((Int, Int), (Int, Int), Int)]
                 }

data State = Won | Play deriving Eq

gen :: [Tile] -> Array (Int, Int) Tile -> [Int] -> (Array (Int, Int) Tile, [Int])
gen [] board (r:rs) = scramble (scrambleSrc board r) rs
gen seeds board (r:r1:rs) = let
  (as, b@(Tile i@(x, y) w):bs) = splitAt (mod r $ length seeds) seeds
  exits = [((x+dx, y+dy), dj) | dj@(dx,dy) <- [(1,0),(0,-1),(-1,0),(0,1)], let
    j = (x+dx, y+dy) in inRange bnds j && (board!j) == Blank && (i /= srcTop || dx == 0)]
  in if null exits then gen (as ++ bs) board (r1:rs) else let
    (j, dj@(dx,dy)) = exits!!(r1 `mod` length exits)
    augT = Tile i (dj:w)
    newT = Tile j [(-dx, -dy)]
    in gen ((augT:newT:as) ++ bs) (board // [(i, augT), (j, newT)]) rs

scramble board rs = let
  f i r | isSrc i   = (i, board!i)
        | otherwise = (i, iterate rot (board!i) !! (r `mod` 4))
  in (array bnds $ zipWith f (range bnds) rs, drop (rangeSize bnds) rs)

scrambleSrc board r = iterate srcRot board !! (r `mod` 4)

followLive game = let
  board = Main.board game
  f [] live n = (live, n)
  f (i@(x, y):is) live n = if (live!i) then f is live n else let
    js = [(x + dx, y + dy) | (dx,dy) <- ways (board!i), let j = (x+dx, y+dy) in inRange bnds j && ((-dx, -dy) `elem` ways (board!j)) && not (live!j)]
    in f (is ++ js) (live // [(i, True)]) (n + 1)
  (live, n) = f [srcBot] (listArray bnds $ repeat False) 0
  in game { live = live, state = if n == rangeSize bnds then Won else Play }

data Event = KeyDown Int | Click Int Int

rot (Tile i w) = Tile i $ map (\(x, y) -> if y /= 0 then (-y, 0) else (0, x)) w

srcRot board = let
  Tile _ w = rot $ Tile (0, 0) $ delete (0, -1) (ways $ board!srcBot) ++
    filter (== (0, -1)) (ways $ board!srcTop)
  in board // [(srcTop, Tile srcTop $ (0,  1) : filter (== (0, -1)) w),
               (srcBot, Tile srcBot $ (0, -1) : filter (/= (0, -1)) w)]

newPackets board i = [(i, dj, 0) | dj <- ways (board!i)]

initGame rs = let
  top = Tile srcTop [(0, 1)]
  bot = Tile srcBot [(0, -1)]
  (board, rs1) = gen [top, bot] ((listArray bnds $ repeat Blank) //
    [(srcTop, top), (srcBot, bot)]) rs
  in followLive $ Game board undefined Play rs1 []

handle game@(Game board live state _ packets) (Click mx my:_) = let
  i = (mx `div` 32, my `div` 32)
  in if inRange bnds i then
    if state == Play then (followLive $
      if isSrc i then game { board = srcRot board }
      else game { board = board // [(i, rot $ board!i)] }, True)
    else (game { packets = packets ++ newPackets board i }, False)
  else (game, False)
handle game (KeyDown 113:_) = (initGame (rands game), True)
handle game _ = (game, False)

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

main = withElems ["body", "canvas", "message"] $ \[body, canvasE, msg] -> do
  evq <- newMVar [KeyDown 113]
  canvasE  `onEvent` OnMouseDown $ \_button (x, y) -> do
    q <- takeMVar evq
    putMVar evq (q ++ [Click x y])
  body `onEvent` OnKeyDown $ \_k -> do
    q <- takeMVar evq
    putMVar evq (q ++ [KeyDown _k])
  Just canvas <- getCanvas canvasE
  Just grid <- let (x, y) = snd bnds in createCanvas ((x+1)*32) ((y+1)*32)
  Just buf  <- let (x, y) = snd bnds in createCanvas ((x+1)*32) ((y+1)*32)
  liveEnd <- paint $ rectB (RGB 255 255 0) 9 9 14 14
  deadEnd <- paint $ rectB (RGB 191 191 191) 10 10 13 13
  packet  <- paint $ (color (RGB 0 0 0) $ fill $ circle (16, 16) 5) >>
                     (color (RGB 255 255 255) $ fill $ circle (16, 16) 4)
  render grid $ color (RGB 192 192 192) $ sequence_
    $ [ stroke $ lineB (x*32) 0 0 288 | x <- [1..9]]
    ++ [ stroke $ lineB 0 (y*32) 320 0 | y <- [1..8]]
  seed <- newSeed
  let
    colWire False = color (RGB 255 127 127)
    colWire  True = color (RGB 0 191 0)
    endPic False = deadEnd
    endPic True = liveEnd
    drawTile _ Blank = return ()
    drawTile live (Tile (x,y) w) = let (ox,oy) = (x*32, y*32) in do
      sequence_ [renderOnTop buf $ colWire live $ stroke $
        lineB (ox + 16) (oy + 16) (16 * dx) (16 * dy) | (dx,dy) <- w]
      when (length w == 1) $ renderOnTop buf $ drawB (endPic live) (ox, oy)
    loop game = do
      q <- swapMVar evq []
      let
        (game1@(Game board live state rs packets), isDirty) = handle game q
        adv packet@((x, y), (dx, dy), t) =
          if t < 16 - 1 then [((x, y), (dx, dy), t + 1)] else let
            (x1, y1) = (x + dx, y + dy)
            in [((x1, y1), dj, 0) | dj <- ways $ board!(x1, y1), dj /= (-dx, -dy)]
        in do
          when isDirty $ do
            render buf $ draw grid (0, 0)
            sequence_ [drawTile (live!i) (board!i) | i <- range bnds]
            renderOnTop buf $ let (x,y) = srcTop in
              rectB (RGB 95 95 191) (x * 32 + 9) (y * 32 + 9) 16 48
            setProp msg "innerHTML" (if state == Won then "Solved" else "")
          render canvas $ draw buf (0, 0)
          game2 <- if state == Won then do
            sequence_ [renderOnTop canvas $ drawB packet
              (32*x + 2*t*dx, 32*y + 2*t*dy) | ((x,y), (dx,dy), t) <- packets]
            return $ game1 { packets = (if null packets then
              newPackets board srcBot else concat $ map adv packets) }
          else return game1
          setTimeout 20 $ loop game2
    in loop $ Game undefined undefined undefined (randomRs (0, 2^20 :: Int) seed) undefined
