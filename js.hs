import Control.Concurrent.MVar
import Control.Monad
import Data.Ix
import Data.Array
import Haste
import Haste.Graphics.Canvas

bnds = ((0,0), (3,3)); frameCnt = 8

data Anim = Ready | Solved | Slide { frame :: Int, r0 :: Int, c0 :: Int, r :: Int, c :: Int } deriving Eq

handle board (KeyDown sym) = let
  (r0, c0) = head [i | i <- range bnds, board!i == 16]
  (r, c) = case sym of
      38  -> (r0 + 1, c0)
      40  -> (r0 - 1, c0)
      37  -> (r0, c0 + 1)
      39  -> (r0, c0 - 1)
      _   -> (-1, -1)
  in if inRange bnds (r, c) then Slide 0 r0 c0 r c else Ready

handle board (Click x y) = let
  (r0, c0) = head [i | i <- range bnds, board!i == 16]
  (r, c) = (y `div` 32, x `div` 32)
  in if inRange bnds (r, c) && (r == r0 && abs (c - c0) == 1 || c == c0 && abs (r - r0) == 1) then Slide 0 r0 c0 r c else Ready

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

data Event = KeyDown Int | Click Int Int

main = do
  Just canvasElem <- elemById "canvas"
  Just canvas <- getCanvas canvasElem
  Just body <- elemById "body"
  evq <- newEmptyMVar
  putMVar evq []
  canvasElem  `onEvent` OnMouseDown $ \_button (x, y) -> do
    q <- takeMVar evq
    putMVar evq (q ++ [Click x y])
  body `onEvent` OnKeyDown $ \_k -> do
    q <- takeMVar evq
    putMVar evq (q ++ [KeyDown _k])
  let
    tile x y n = let grey = if n == 16 then 0 else 255 - 15*(n - 1) in color (RGB grey grey grey) $ fill $ rect (fromIntegral x, fromIntegral y) (32 + fromIntegral x, 32 + fromIntegral y)
    animate board Ready = if isSolved board then alert "A WINNER IS YOU!" >> return (board, Solved) else return (board, Ready)
    animate board Solved = return (board, Solved)
    animate board slide@(Slide frame r0 c0 r c) = renderOnTop canvas $ do
      tile (32*c) (32*r) 16
      tile (32*c + 32*(c0 - c) * frame `div` frameCnt)
           (32*r + 32*(r0 - r) * frame `div` frameCnt) (board!(r,c))
      return (if frame == frameCnt - 1 then
        (board // [((r,c), board!(r0,c0)), ((r0,c0), board!(r,c))], Ready) else
        (board, slide{frame = (frame + 1)}))
    loop board anim = do
      render canvas $ sequence_ [tile (32*c) (32*r) (board!(r, c)) | (r, c) <- range bnds]
      (board1, anim1) <- animate board anim
      anim2 <- eventLoop board1 anim1
      if anim2 == Solved then newGame else setTimeout 10 $ loop board1 anim2
    eventLoop board anim = do
      q <- takeMVar evq
      putMVar evq []
      if null q || anim == Solved then return anim else return $ handle board (head q)
    newGame = do
      x <- gen
      loop (array bnds [(i, x!!(4*r + c)) | i@(r, c) <- range bnds]) Ready
    in newGame
