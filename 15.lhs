= 15 Shades of Grey =

Arrange the grey squares so they appear from brightest to darkest going left to
right, top to bottom.

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src="15.js"></script>
<canvas id="canvas" style="border:1px solid black;display:block; margin:auto;"
  width="256" height="256"></canvas>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Source [https://github.com/blynn/chapman[git repo]]:

\begin{code}
import Control.Concurrent.MVar
import Control.Monad
import Data.Array
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

data Event = Ready | Slide Int (Int, Int) (Int, Int) deriving Eq

m = 4; bnds = ((0, 0), (m - 1, m - 1)); frameCnt = 8; sz = 64

parity []     = 0
parity (x:xs) = (if x == m^2 then uncurry (+) $ divMod (length xs) m else 0)
              + length (filter (x>) xs) + parity xs

rndPerm [] _     = []
rndPerm xs seed0 = b:rndPerm (a ++ bs) seed1 where
  (n, seed1) = randomR (0, length xs - 1) seed0
  (a, b:bs)  = splitAt n xs

gen = newSeed >>= \seed -> let z = rndPerm [1..m^2] seed
  in if even $ parity z then return $ listArray bnds z else gen

tile x y n = color (RGB c c c) $ fill $ rect (fromIntegral x, fromIntegral y)
  (fromIntegral $ x + sz, fromIntegral $ y + sz)
  where c | n == m^2  = 0
          | otherwise = 255 - (225 * (n - 1) `div` (m^2 - 1))

main = withElems ["canvas"] $ \[cElem] -> do
  Just canvas <- fromElem cElem
  ev <- newEmptyMVar
  let
    loop = takeMVar ev >>= \(q, b) -> do
      render canvas $ forM_ (assocs b) $ \((r, c), n) -> tile (sz*c) (sz*r) n
      case q of
        Slide frame i0@(r0, c0) i@(r, c) -> do
          renderOnTop canvas $ tile (sz*c) (sz*r) (m^2) >>
            tile (sz*c + sz*(c0 - c) * frame `div` frameCnt)
                 (sz*r + sz*(r0 - r) * frame `div` frameCnt) (b!(r,c))
          if frame == frameCnt - 1
            then putMVar ev (Ready, b // [(i, b!i0), (i0, b!i)]) >> loop
            else putMVar ev (Slide (frame + 1) i0 i, b) >>
              void (setTimer (Once 10) loop)
        Ready | [1..m^2] == elems b -> alert "You win!" >> newGame
              | otherwise           -> putMVar ev (q, b)
    newGame = gen >>= putMVar ev . (,) Ready >> loop
    try q i0@(r0, c0) i@(r, c) b
      | inRange bnds (r, c) && (c - c0)^2 + (r - r0)^2 == 1 =
        putMVar ev (Slide 0 i0 i, b) >> when (q == Ready) loop
      | otherwise = putMVar ev (q, b)
  cElem `onEvent` MouseDown $
    \(MouseData (x, y) _ _) -> takeMVar ev >>= \(q, b) ->
      try q (head [i | i <- range bnds, b!i == m^2]) (y `div` sz, x `div` sz) b
  documentBody `onEvent` KeyDown $ \k -> takeMVar ev >>= \(q, b) -> let
    f (r, c) = try q (r0, c0) (r, c) b >> preventDefault
    (r0, c0) = head [i | i <- range bnds, b!i == m^2]
    in case k of
      38  -> f (r0 + 1, c0)
      40  -> f (r0 - 1, c0)
      37  -> f (r0, c0 + 1)
      39  -> f (r0, c0 - 1)
      _   -> putMVar ev (q, b)
  newGame
\end{code}
