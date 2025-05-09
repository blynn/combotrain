= Breakthrough =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src="breakthrough.js"></script>
<canvas id="canvas" style="border:1px solid black;display:block;margin:auto;" width="320" height="320"></canvas>
<div style="text-align:center" id="message">
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Breakthrough was invented by Dan Troyka [http://en.wikipedia.org/wiki/Breakthrough_(board_game)[Rules]].

We use https://en.wikipedia.org/wiki/Bitboard[bitboards]; one for the black
pieces and one for the white.

\begin{code}
{-# LANGUAGE LambdaCase #-}
import Control.Arrow
import Control.Monad
import Data.Bits
import Data.IORef
import Data.List
import Data.Maybe
import Data.Tree
import Data.Word
import System.Random
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Haste.Graphics.AnimationFrame

data Game = Game { white :: Word64
                 , black :: Word64
                 , won :: Bool
                 , player :: Int
                 } deriving Show

initGame = Game 0xffff 0xffff000000000000 False 0

score game = if won game then player game * 2048 else
  popCount (white game) - popCount (black game)

omitWith op (ms:mss) = m : omit m mss where
  oppest = foldl1' (\a b -> if op a b then a else b)
  m = oppest ms
  omit _   [] = []
  omit pot (ns:nss) | any (`op` pot) ns = omit pot nss
                    | otherwise = n : omit n nss
                    where n = oppest ns

maximize' :: Tree Game -> [Int]
maximize' (Node leaf [])   = [score leaf]
maximize' (Node _    kids) = omitWith (<=) $ minimize' <$> kids

minimize' :: Tree Game -> [Int]
minimize' (Node leaf [])   = [score leaf]
minimize' (Node _    kids) = omitWith (>=) $ maximize' <$> kids

best gs = snd $ foldl1' go $ zip (maximum . maximize' . prune 4 . gameTree <$> gs) gs
  where
  go a b = if fst a < fst b then a else b

gameTree = unfoldTree (\x -> (x, nextMoves x))

edgeL = 0x8080808080808080
edgeR = 0x0101010101010101

nextMoves game
  | won game = []
  | player game == 0 = movesW 1
  | otherwise = movesB 1
  where
  b = black game
  w = white game
  movesW = \case
    0 -> []
    k -> (++ movesW (2*k)) $ if w .&. k /= 0
      then let
        idxs
          | k .&. edgeL /= 0 = [7, 8]
          | k .&. edgeR /= 0 = [8, 9]
          | otherwise        = [7, 8, 9]
      in [Game { white = xor w (k .|. k'), black = b .&. complement (b .&. k'), won = k >= 2^48 , player = 1 }
        | i <- idxs, let k' = shiftL k i, w .&. k' == 0, i /= 8 || b.&. k' == 0]
      else []
  movesB = \case
    0 -> []
    k -> (++ movesB (k * 2)) $ if b .&. k /= 0
      then let
        idxs
          | k .&. edgeL /= 0 = [8, 9]
          | k .&. edgeR /= 0 = [7, 8]
          | otherwise        = [7, 8, 9]
      in [Game { black = xor b (k .|. k'), white = w .&. complement (w .&. k'), won = k < 2^16 , player = 0 }
        | i <- idxs, let k' = shiftR k i, b .&. k' == 0, i /= 8 || w.&. k' == 0]
      else []

prune 0 (Node a _) = Node a []
prune n (Node a kids) = Node a $ map (prune (n - 1)) kids

diff g0 g1 = uncard <$> if player g0 == 0
  then filter (testBit $ white g0 `xor` white g1) [0..63]
  else reverse $ filter (testBit $ black g0 `xor` black g1) [0..63]

movesFrom (x, y) g = [ (x + dx, y - 1) | dx <- [-1,0,1], let k = card (x + dx) (y - 1), not $ testBit (white g) k, dx /= 0 || not (testBit (black g) k)]

uncard i = (x, 7-y) where (y, x) = divMod i 8

box :: Int -> Int -> Int -> Int -> Picture ()  -- Why is this needed?
box x y dx dy = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x+dx), fromIntegral (y+dy))

sqColor False = RGB 191 191 191
sqColor True  = RGB 255 255 255

drawB pic x y = draw pic (fromIntegral x, fromIntegral y)

playerName 0 = "White"
playerName 1 = "Black"

shuffleIO [] = return []
shuffleIO xs = getStdRandom (randomR (0, length xs - 1)) >>= \n ->
  let (a, b:bs) = splitAt n xs in (b:) <$> shuffleIO (a ++ bs)

card x y = 8*(7-y) + x
rice x y = 2^card x y :: Word64

sz = 40

main = withElems ["canvas", "message"] $ \[canvasE, msg] -> do
  Just canvas <- fromElem canvasE
  whitePiece <- createCanvas sz sz
  renderOnTop whitePiece $ color (RGB 255 255 255) $ fill $ circle (20, 20) 10
  renderOnTop whitePiece $ color (RGB 0 0 0) $ stroke $ circle (20, 20) 11
  blackPiece <- createCanvas sz sz
  renderOnTop blackPiece $ color (RGB 0 0 0) $ fill $ circle (20, 20) 11

  fromCan <- createCanvas sz sz
  render fromCan $ color (RGB 127 15 15) $ sequence_
    [ box 0 0 5 40, box 0 0 40 5, box 35 0 40 40, box 0 35 40 40 ]
  toCan <- createCanvas sz sz
  render toCan $ color (RGBA 0 191 0 0.3) $ box 0 0 sz sz

  boardCan <- createCanvas 320 320
  sequence_ [renderOnTop boardCan $ color (sqColor (mod (x + y) 2 == 0)) $ box (x*sz) (y*sz) sz sz | x <- [0..7], y <- [0..7]]
  buf <- createCanvas 320 320
  
  gameRef <- newIORef initGame
  busyRef <- newIORef False
  selRef <- newIORef Nothing

  let
    renderPiece c p (x,y) = renderOnTop c $ draw (if p == 0 then whitePiece else blackPiece) (fromIntegral x, fromIntegral y)
    drawGame game = do
      let
        w = white game
        b = black game
      sequence_ $ (render buf $ draw boardCan (0, 0)) : [renderPiece buf (if testBit w i then 0 else 1) (x*sz, y*sz) | i <- [0..63], testBit (w .|. b) i, let (y, x) = first (7-) $ divMod i 8]
      render canvas $ draw buf (0, 0)
      setProp msg "innerHTML" $ if won game
        then playerName (1 - player game) ++ " wins"
        else playerName (player game) ++ " to move"
    anim game frame m@[(x0, y0), (x1, y1)] _
      | frame == 8 = let
        game1 = if player game == 0
          then game { white = white game `xor` (rice x0 y0 .|. rice x1 y1), won = y1 == 0, black = black game .&. complement (rice x1 y1), player = 1 - player game }
          else game { black = black game `xor` (rice x0 y0 .|. rice x1 y1), won = y1 == 7, white = white game .&. complement (rice x1 y1), player = 1 - player game }
        in do
          drawGame game1
          if not (won game1) && player game1 == 1
            then void $ requestAnimationFrame $ \_ -> do
              gs <- shuffleIO $ nextMoves game1
              void $ requestAnimationFrame $ anim game1 0 $ diff game1 $ best gs
            else do
              writeIORef busyRef False
              writeIORef gameRef game1
              writeIORef selRef Nothing

      | otherwise = let f x0 x1 frame = x0 * sz + (x1 - x0) * sz * frame `div` 8 in do
        drawGame game
          { white = white game .&. complement (rice x0 y0)
          , black = black game .&. complement (rice x0 y0)
          }
        renderPiece canvas (player game) (f x0 x1 frame, f y0 y1 frame)
        void $ requestAnimationFrame $ anim game (frame + 1) m

  void $ canvasE  `onEvent` MouseDown $ \m -> do
    busy <- readIORef busyRef
    unless busy $ do
      game <- readIORef gameRef
      let (bx, by) = mouseCoords m
      when (not (won game) && player game == 0) $ let
        sel@(x, y) = (div bx sz, div by sz)
        w = white game
        in do
          render canvas $ draw buf (0, 0)
          readIORef selRef >>= \case
            Nothing -> when (w .&. rice x y /= 0) $ do
              renderOnTop canvas $ drawB fromCan (x*sz) (y*sz)
              sequence_ [renderOnTop canvas $
                drawB toCan (x1*sz) (y1*sz) | (x1, y1) <- movesFrom sel game]
              writeIORef selRef $ Just sel
            Just sel0
              | sel `elem` movesFrom sel0 game -> do
                requestAnimationFrame $ anim game 0 [sel0, sel]
                writeIORef busyRef True
              | otherwise -> writeIORef selRef Nothing

  void $ documentBody `onEvent` KeyDown $ \k -> when (keyCode k == 113) $ do
    busy <- readIORef busyRef
    unless busy $ do
      writeIORef gameRef initGame
      drawGame =<< readIORef gameRef

  drawGame =<< readIORef gameRef
\end{code}
