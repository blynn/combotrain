import Control.Concurrent.MVar
import Control.Monad
import Data.Ix
import Data.Array
import Haste
import Haste.Graphics.Canvas

bnds = ((0,0), (3,3)); frameCnt = 8

data Anim = Done | Slide { frame :: Int, r0 :: Int, c0 :: Int, r :: Int, c :: Int } deriving Eq

key board sym = let
  (r0, c0) = head [i | i <- range bnds, board!i == 16]
  (r, c) = case sym of
      38  -> (r0 + 1, c0)
      40  -> (r0 - 1, c0)
      37  -> (r0, c0 + 1)
      39  -> (r0, c0 - 1)
      _   -> (-1, -1)
  in if inRange bnds (r, c) then Slide 0 r0 c0 r c else Done

parity [] = 0
parity (16:xs) = parity xs + (length xs `div` 4)
parity (x:xs) = sum [1 | y <- xs, x > y] + parity xs

gen = do
  seed <- newSeed
  let
    f [x] _ = [x]
    f xs seed = let
      (n, seed1) = randomR (0, length xs - 1) seed
      (a, b:bs) = splitAt n xs in b:f (a ++ bs) seed1
    z = f [1..16] seed
    in if parity z `mod` 2 == 0 then return z else gen

isSolved board = and [board!i == 4*r + c + 1 | i@(r, c) <- range bnds]

main = do
  Just canvas <- getCanvasById "canvas"
  Just body <- elemById "body"
  evq <- newEmptyMVar
  putMVar evq []
  body `onEvent` OnKeyDown $ \_k -> do
    q <- takeMVar evq
    putMVar evq (q ++ [_k])
  let
    tile x y n = let grey = if n == 16 then 0 else 255 - 15*(n - 1) in color (RGB grey grey grey) $ fill $ rect (fromIntegral x, fromIntegral y) (32 + fromIntegral x, 32 + fromIntegral y)
    animate board Done = return (board, Done)
    animate board slide@(Slide frame r0 c0 r c) = renderOnTop canvas $ do
      tile (32*c) (32*r) 16
      tile (32*c + 32*(c0 - c) * frame `div` frameCnt)
           (32*r + 32*(r0 - r) * frame `div` frameCnt) (board!(r,c))
      return (if frame == frameCnt - 1 then
        (board // [((r,c), board!(r0,c0)), ((r0,c0), board!(r,c))], Done) else
        (board, slide{frame = (frame + 1)}))
    loop board anim = do
      render canvas $ sequence_ [tile (32*c) (32*r) (board!(r, c)) | (r, c) <- range bnds]
      (board1, anim1) <- animate board anim
      (quit, anim2) <- eventLoop board1 anim1
      if quit then newGame else setTimeout 10 $ loop board1 anim2
    eventLoop board anim = do
      if isSolved board then alert "A WINNER IS YOU!" >> return (True, anim) else do
        q <- takeMVar evq
        putMVar evq []
        if null q then return (False, anim) else
          if anim /= Done then return (False, anim) else return (False, key board (head q))
    newGame = do
      shuf <- gen
      loop (array bnds [(i, shuf!!(4*r + c)) | i@(r, c) <- range bnds]) Done
    in newGame
