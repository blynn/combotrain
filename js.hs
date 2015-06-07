import Control.Concurrent.MVar
import Control.Monad
import Control.Applicative
import Data.Array
import Haste
import Haste.Graphics.Canvas

bnds = ((0,0), (3,3)); frameCnt = 8; sz = 64

data Anim = Ready | Solved |
   Slide { frame :: Int, r0 :: Int, c0 :: Int, r :: Int, c :: Int } deriving Eq
data Event = KeyDown Int | Click Int Int

handle board ev = let
  (r0, c0) = head [i | i <- range bnds, board!i == 16]
  (r , c ) = case ev of
    KeyDown sym -> case sym of
      38  -> (r0 + 1, c0)
      40  -> (r0 - 1, c0)
      37  -> (r0, c0 + 1)
      39  -> (r0, c0 - 1)
      _   -> (-1, -1)
    Click x y -> (y `div` sz, x `div` sz)
  in if inRange bnds (r, c) && (abs (c - c0) + abs (r - r0) == 1)
    then Slide 0 r0 c0 r c else Ready

parity      [] = 0
parity (16:xs) = (length xs `div` 4)     + parity xs
parity ( x:xs) = length (filter (x>) xs) + parity xs

gen = do
  seed <- newSeed
  let
    f [x] _ = [x]
    f xs seed = let
      (n, seed1) = randomR (0, length xs - 1) seed
      (a, b:bs) = splitAt n xs
      in b:f (a ++ bs) seed1
    z = f [1..16] seed
  if even $ parity z then return $ listArray bnds z else gen

grey n = color $ RGB c c c
  where c = case n of 16 -> 0
                      _  -> 255 - 15*(n - 1)

main = withElems ["body", "canvas"] $ \[body, canvasElem] -> do
  Just canvas <- getCanvas canvasElem
  evq <- newMVar []
  canvasElem  `onEvent` OnMouseDown $
    \_ (x, y) -> takeMVar evq >>= putMVar evq . (++ [Click x y])
  body `onEvent` OnKeyDown $
    \k -> takeMVar evq >>= putMVar evq . (++ [KeyDown k])
  let
    tile x y n = grey n $ fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral $ x + sz, fromIntegral $ y + sz)
    animate board Ready = if [1..16] == elems board
      then alert "A WINNER IS YOU!" >> return (board, Solved)
      else return (board, Ready)
    animate board Solved = return (board, Solved)
    animate board slide@(Slide frame r0 c0 r c) = renderOnTop canvas $ do
      tile (sz*c) (sz*r) 16
      tile (sz*c + sz*(c0 - c) * frame `div` frameCnt)
           (sz*r + sz*(r0 - r) * frame `div` frameCnt) (board!(r,c))
      return $ if frame == frameCnt - 1
        then (board // [((r,c), board!(r0,c0)), ((r0,c0), board!(r,c))], Ready)
        else (board, slide{frame = frame + 1})
    loop board anim = do
      render canvas $ sequence_ $ (\((r, c), n) -> tile (sz*c) (sz*r) n) <$> assocs board
      (board1, anim1) <- animate board anim
      if anim1 == Solved then newGame else do
        q <- swapMVar evq []
        setTimeout 10 $ loop board1 $ if null q
          then anim1 else handle board1 (head q)
    newGame = gen >>= flip loop Ready
  newGame
