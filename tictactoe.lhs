= Tic-tac-toe =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src="tictactoe.js"></script>
<canvas id="canvas" style="border:1px solid black;display:block;margin:auto;" width="192" height="192"></canvas>
<div style="text-align:center;">
<div id="message"></div>
<input type="checkbox" id="noab">Disable
<a
href="http://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning">alpha-beta
pruning</a>.
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

\begin{code}
import Data.Array
import Data.Bool
import Data.IORef
import Data.List
import Data.Ord
import Data.Tree
import Control.Monad
import System.Random
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Haste.JSString (pack)

sz = 64; bd = 4; bnds = ((0,0), (2,2))
moveRandomly = False

data Status = Draw | Won | Play deriving Eq
data Game = Game (Array (Int, Int) Int) Status Int

initGame = Game (listArray bnds $ repeat 0) Play (-1)

goals = [join (,) <$> [0..2], (,) <*> (2-) <$> [0..2]]  -- Diagonals.
        ++ ((<$> [0..2]) .      (,) <$> [0..2])  -- Rows and columns.
        ++ ((<$> [0..2]) . flip (,) <$> [0..2])

move board0 player i
  | or $ all ((== player) . (board!)) <$> goals = Game board Won  player
  | 0 `notElem` elems board                     = Game board Draw player
  | otherwise                                   = Game board Play $ -player
  where board = board0 // [(i, player)]

nextMoves game@(Game board status p) = (game, case status of
  Play -> [move board p i | i <- indices board, board!i == 0]
  _    -> [])

gameTree = unfoldTree nextMoves

score (Game _ Won n) = n
score _              = 0

maximize (Node leaf [])   = leaf
maximize (Node _    kids) = maximum $ minimize <$> kids

minimize (Node leaf [])   = leaf
minimize (Node _    kids) = minimum $ maximize <$> kids

best = maximumBy $ comparing $ minimize . fmap score . gameTree

omitWith op ((g, ns):nss) = let
  omit _   [] = []
  omit pot ((g, ns):nss) | any (`op` pot) ns = omit pot nss
                         | otherwise = (g, last ns) : omit (last ns) nss
  in (g, last ns) : omit (last ns) nss

maximize' :: Tree Game -> [(Game, Int)]
maximize' (Node leaf [])   = [(undefined, score leaf)]
maximize' (Node _    kids) = omitWith (<=)
  [(rootLabel k, snd <$> minimize' k) | k <- kids]

minimize' :: Tree Game -> [(Game, Int)]
minimize' (Node leaf [])   = [(undefined, score leaf)]
minimize' (Node _    kids) = omitWith (>=)
  [(rootLabel k, snd <$> maximize' k) | k <- kids]

bestAB ms = fst $ last . maximize' $ Node undefined $ map gameTree ms

f = fromIntegral

oblong x y w h = fill $ rect (f x, f y) (f $ x + w, f $ y + h)

main = withElems ["canvas", "message", "noab"] $ \[cElem, message, noab] -> do
  xo <- loadBitmap $ pack "xo.png"
  Just canvas <- fromElem cElem
  gameVar <- newIORef initGame
  let
    shuffleIO [] = return []
    shuffleIO xs = getStdRandom (randomR (0, length xs - 1)) >>= \n ->
      let (a, b:bs) = splitAt n xs in (b:) <$> shuffleIO (a ++ bs)

    sq ((x, y), p) = do
      -- Draw borders.
      when (x /= 0) $ oblong (x * sz)           (y * sz) bd sz
      when (x /= 2) $ oblong (x * sz + sz - bd) (y * sz) bd sz
      when (y /= 0) $ oblong (x * sz)           (y * sz) sz bd
      when (y /= 2) $ oblong (x * sz) (y * sz + sz - bd) sz bd
      -- Draw nought or cross when present.
      when (p /= 0) $ drawClipped xo (f $ x * sz, f $ y * sz) $
        Rect (f $ bool 0 sz $ p > 0) 0 (f sz) (f sz)

    aiMove game = do
      shuffledMoves <- shuffleIO $ snd $ nextMoves game
      disableAB <- ("true" ==) <$> getProp noab "checked"
      return $ if moveRandomly then head shuffledMoves else
        bool bestAB best disableAB shuffledMoves

    go game = writeIORef gameVar game >> update

    update = do
      game@(Game board status player) <- readIORef gameVar
      render canvas $ mapM_ sq $ assocs board
      setProp message "innerHTML" $ case status of
        Won  -> ("X.O"!!(player + 1)) : " wins"
        Draw -> "Draw"
        Play -> if player == -1 then "X to move" else "Thinking..."
      when (player == 1 && status == Play) $ void $
        setTimer (Once 1) $ aiMove game >>= go  -- Delay for redraw.

  _ <- cElem `onEvent` MouseDown $ \(MouseData (x, y) _ _) -> do
    Game board status player <- readIORef gameVar
    let i = (x `div` sz, y `div` sz) in when (status == Play && player == -1
       && inRange bnds i && board!i == 0) $ go $ move board player i

  _ <- documentBody `onEvent` KeyDown $ \k -> when (keyCode k == 113) $ go initGame

  update
\end{code}
