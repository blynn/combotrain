= Peg Solitaire =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src="peg.js"></script>
<canvas id="canvas" width="280" height="280" style="display:block;margin:auto;">
</canvas>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Half our imports are for the user interface:

\begin{code}
import Control.Monad
import Data.Bool
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
\end{code}

Setting up the board and acting on the selection of a row and column is
just a handful of lines:

\begin{code}
initState :: Map (Int, Int) Bool
initState = let f x = (x - 3)^2 <= 1 in M.fromList
  [((r, c), r /= 3 || c /= 3) | r <- [0..6], c <- [0..6], f r || f c]

act :: (Map (Int, Int) Bool, Maybe (Int, Int)) -> (Int, Int)
    -> (Map (Int, Int) Bool, Maybe (Int, Int))
act (st, sel) p@(r, c)
  | M.notMember p st = (st, Nothing)
  | Nothing <- sel   = (st, if st M.! p then Just p else Nothing)
  | p' == p          = (st, Nothing)
  | st M.! p         = (st, Just p)
  | (r' - r)^2 + (c - c')^2 == 4, st M.! m = (M.insert p' False $
    M.insert p True $ M.insert m False st, Just p)
  | otherwise        = (st, Nothing)
  where Just p'@(r', c') = sel
        m = (div (r + r') 2, div (c + c') 2)
\end{code}

The rest of the program deals with drawing the board and handling user input:

\begin{code}
sz :: Int
sz = 40
rad :: Double
rad = 12

spot :: (Int, Int) -> Double -> Picture ()
spot (r, c) t = let m = div sz 2 in fill $
  circle (fromIntegral (sz*c + m), fromIntegral (sz*r + m)) t

pegPic :: ((Int, Int), Bool) -> Picture ()
pegPic (p, b) = color (RGB (bool 0 255 b) 0 0) $ spot p rad

victory :: Canvas -> Map (Int, Int) Bool -> IO ()
victory canvas st = when
  (M.filterWithKey (const id) st == M.singleton (3, 3) True) $ do
  let
    m = div sz 2
    [ox, oy] = fromIntegral <$> [sz*3 + m, sz*3 + m]
  renderOnTop canvas $ color (RGB 255 255 255) $ sequence_ [
    fill $ circle (ox - rad/4, oy - rad/4) 1.5,
    fill $ circle (ox + rad/4, oy - rad/4) 1.5,
    lineWidth 2 $ stroke $ arc (ox, oy) (rad/2) (1/6*pi) (5/6*pi)]

paint :: Canvas -> (Map (Int, Int) Bool, Maybe (Int, Int)) -> IO ()
paint canvas (st, sel) = do
  render canvas $ case sel of
    Just p -> color (RGB 127 255 255) $ spot p $ rad + 3
    Nothing -> pure ()
  void $ renderOnTop canvas $ mapM pegPic $ M.assocs st
  victory canvas st

main :: IO ()
main = withElems ["canvas"] $ \[cElem] -> do
  Just canvas <- fromElem cElem
  ref <- newIORef (initState, Nothing)
  let refresh = readIORef ref >>= paint canvas
  refresh
  void $ cElem `onEvent` MouseDown $ \(MouseData (x, y) _ _) -> do
    modifyIORef ref (`act` (div y sz, div x sz))
    refresh
\end{code}
