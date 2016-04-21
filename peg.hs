import Control.Monad
import Data.Bool
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!), Map)
import Data.Maybe
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

sz = 32
m = div sz 2
rad = 10

spot (r, c) t = fill $
  circle (fromIntegral (sz*c + m), fromIntegral (sz*r + m)) $ fromIntegral t

pegPic (p, b) = color (RGB (bool 0 255 b) 0 0) $ spot p rad

ctr x = (x - 3)^2 <= 1

initState = foldl' (\m k -> M.insert k (k /= (3, 3)) m) M.empty
  [(r, c) | r <- [0..6], c <- [0..6], ctr r || ctr c]

main = withElems ["canvas"] $ \[cElem] -> do
  Just canvas <- fromElem cElem
  ref <- newIORef (initState, Nothing)

  let
    draw = do
      (st, sel) <- readIORef ref
      render canvas $ mapM pegPic $ M.assocs st
      unless (isNothing sel) $ let Just p = sel in mapM_ (renderOnTop canvas)
        [color (RGB 0 0 0) $ spot p $ rad + 2, pegPic (p, st!p)]
  draw

  cElem `onEvent` MouseDown $ \(MouseData (x, y) _ _) -> do
    (st, sel) <- readIORef ref
    let
      p@(r, c) = (div y sz, div x sz)
      Just p'@(r', c') = sel
      f | M.notMember p st = (st, Nothing)
        | isNothing sel    = (st, if st!p then Just p else Nothing)
        | p' == p          = (st, Nothing)
        | st!p             = (st, Just p)
        | (r' - r)^2 + (c - c')^2 == 4 = (M.insert p' False $ M.insert p True $
          M.insert (div (r + r') 2, div (c + c') 2) False st, Just p)
        | otherwise        = (st, Nothing)
    writeIORef ref f
    draw
