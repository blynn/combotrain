-- We can only receive mouse and keyboard events when none of our code is
-- running or during the timeout, thus we could use IORef instead of MVar.
--
-- Our MVar contains Ready except when there is a pending timeout, which
-- explains some of the logic.
import Control.Concurrent.MVar
import Control.Applicative
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
rndPerm xs seed0 = let (n, seed1) = randomR (0, length xs - 1) seed0
                       (a, b:bs) = splitAt n xs
                       in b:rndPerm (a ++ bs) seed1

gen = newSeed >>= \seed -> let z = rndPerm [1..m^2] seed
  in if even $ parity z then return $ listArray bnds z else gen

tile x y n = color (RGB c c c) $ fill $ rect (fromIntegral x, fromIntegral y)
  (fromIntegral $ x + sz, fromIntegral $ y + sz)
  where c = if n == m^2 then 0 else 255 - (225 * (n - 1) `div` (m^2 - 1))

main = withElems ["canvas"] $ \[cElem] -> do
  Just canvas <- getCanvas cElem
  ev <- newEmptyMVar
  let
    loop = takeMVar ev >>= \(q, b) -> do
      render canvas $ forM_ (assocs b) $ \((r, c), n) -> tile (sz*c) (sz*r) n
      if [1..m^2] == elems b then alert "You win!" >> newGame else case q of
        Slide frame i0@(r0, c0) i@(r, c) -> do
          renderOnTop canvas $ tile (sz*c) (sz*r) (m^2) >>
            tile (sz*c + sz*(c0 - c) * frame `div` frameCnt)
                 (sz*r + sz*(r0 - r) * frame `div` frameCnt) (b!(r,c))
          if frame == frameCnt - 1
            then putMVar ev (Ready, b // [(i, b!i0), (i0, b!i)]) >> loop
            else putMVar ev (Slide (frame + 1) i0 i, b) >>
              void (setTimer (Once 10) loop)
        _ -> putMVar ev (q, b)
    newGame = gen >>= putMVar ev . (,) Ready >> loop
    try q i0@(r0, c0) i@(r, c) b =
      if inRange bnds (r, c) && abs (c - c0) + abs (r - r0) == 1
        then putMVar ev (Slide 0 i0 i, b) >> when (q == Ready) loop
        else putMVar ev (q, b)
  cElem `onEvent` MouseDown $
    \(MouseData (x, y) _ _) -> takeMVar ev >>= \(q, b) ->
      try q (head [i | i <- range bnds, b!i == m^2]) (y `div` sz, x `div` sz) b
  documentBody `onEvent` KeyDown $ \k -> takeMVar ev >>= \(q, b) -> let
    (r0, c0) = head [i | i <- range bnds, b!i == m^2]
    (r , c ) = case k of 38  -> (r0 + 1, c0)
                         40  -> (r0 - 1, c0)
                         37  -> (r0, c0 + 1)
                         39  -> (r0, c0 - 1)
                         _   -> (-1, -1)
    in try q (r0, c0) (r, c) b
  newGame
