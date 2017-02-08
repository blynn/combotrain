= Breakthrough =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src="breakthrough.js"></script>
<canvas id="canvas" style="border:1px solid black;display:block;margin:auto;" width="320" height="320"></canvas>
<div style="text-align:center" id="message">
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Breakthrough was invented by Dan Troyka [http://en.wikipedia.org/wiki/Breakthrough_(board_game)[Rules]].

\begin{code}
import Control.Monad
import Data.Array
import Data.Maybe
import Data.Tree
import Haste
import Haste.Concurrent hiding ((!))
import qualified Haste.Concurrent as H
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

bnds = ((0,0), (7,7)); sz = 40

data Event = Mo (Int, Int) | Ke Int
data State = Won | Play deriving Eq
data Game = Game { board :: Array (Int, Int) Int
                 , state :: State
                 , player :: Int
                 , selection :: Maybe (Int, Int)
                 , anim :: Maybe (Int, ((Int, Int), (Int, Int)))
                 , lastMove :: ((Int, Int), (Int, Int))
                 }

initRow y | y <= 1 = -1
          | y >= 6 = 1
          | True   = 0

initBoard = array bnds [(i, initRow y) | i@(x,y) <- range bnds]

initGame = Game initBoard Play 1 Nothing Nothing undefined

score game = if state game == Won then player game * (-1024) else
  (-1) * sum [(board game)!i | i <- range bnds]

omitWith op ((g, ns):nss) = let
  omit pot [] = []
  omit pot ((g, ns):nss) | or $ map (`op` pot) ns = omit pot nss
                         | otherwise = (g, last ns) : omit (last ns) nss
  in (g, last ns) : omit (last ns) nss

maximize' :: Tree Game -> [(Game, Int)]
maximize' (Node leaf []) = [(undefined, score leaf)]
maximize' (Node g kids) = omitWith (<=) $
  [(rootLabel k, map snd $ minimize' k) | k <- kids]

maximize = last . maximize'

minimize' :: Tree Game -> [(Game, Int)]
minimize' (Node leaf []) = [(undefined, score leaf)]
minimize' (Node g kids) = omitWith (>=) $
  [(rootLabel k, map snd $ maximize' k) | k <- kids]

best game ms = lastMove $ fst $ maximize $ prune 4 $
  Node game (map (gameTree . move game) ms)

gameTree = unfoldTree (\x -> (x, nextNodes x))

nextMoves game = if state game == Play then [(i, dst) | i <- range bnds, (board game)!i == player game, dst <- movesFrom i game] else []

nextNodes game = map (move game) $ nextMoves game

prune 0 (Node a _) = Node a []
prune n (Node a kids) = Node a $ map (prune (n - 1)) kids

box :: Int -> Int -> Int -> Int -> Picture ()  -- Why is this needed?
box x y dx dy = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x+dx), fromIntegral (y+dy))

sqColor False = RGB 191 191 191
sqColor True  = RGB 255 255 255

drawB pic x y = draw pic (fromIntegral x, fromIntegral y)

playerName   1  = "White"
playerName (-1) = "Black"

movesFrom (x, y) game = let
  b = board game
  p = player game
  in [i1 | dx <- [-1, 0, 1], let i1 = (x + dx, y - p), inRange bnds i1, b!i1 /= p, dx /= 0 || b!i1 == 0]

move game (i0, i1@(_, y1)) = let
  p = player game
  nextBoard = board game // [(i0, 0), (i1, p)]
  nextState = if (p == 1 && y1 == 0) || (p == -1 && y1 == 7) then Won else Play
  in Game nextBoard nextState (if nextState == Won then p else -p) Nothing Nothing (i0, i1)

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
  sequence_ $ [renderOnTop boardCan $ color (sqColor (mod (x + y) 2 == 0)) $ box (x*sz) (y*sz) sz sz | (x, y) <- range bnds]
  buf <- createCanvas 320 320

  ev <- newEmptyMVar
  canvasE  `onEvent` MouseDown $ \m -> concurrent $ putMVar ev $ Mo $ mouseCoords m
  documentBody `onEvent` KeyDown $ \k -> concurrent $ putMVar ev $ Ke $ keyCode k

  seed <- newSeed
  seedV <- newMVar seed

  let
    renderPiece c p (x,y) = renderOnTop c $ draw (if p == 1 then whitePiece else blackPiece) (fromIntegral x, fromIntegral y)

    randomRIO range = do
      seed <- takeMVar seedV
      let (r, seed1) = randomR range seed in do
        putMVar seedV seed1
        return r

    shuffleIO [] = return []
    shuffleIO xs = do
      n <- randomRIO (0, length xs - 1)
      let (a, b:bs) = splitAt n xs in do 
        ys <- shuffleIO (a ++ bs)
        return (b:ys)

    drawGame game = do
      sequence_ $ (render buf $ draw boardCan (0, 0)) : [renderPiece buf p (x*sz, y*sz) | i@(x, y) <- range bnds, let p = (board game)!i, p /= 0]
      render canvas $ draw buf (0, 0)
      setProp msg "innerHTML" $ playerName (player game) ++ case state game of
        Play -> " to move"
        Won -> " wins"

    loop game = if isNothing $ anim game then let sel0 = selection game in do
      e <- takeMVar ev
      case e of
        Mo (bx, by) -> when (state game == Play) $ let
          i@(x, y) = (div bx sz, div by sz)
          sel = if (board game)!i == player game then Just i else Nothing
          in when (inRange bnds i) $ do
            render canvas $ draw buf (0, 0)
            if sel0 == Nothing then do
              unless (sel == Nothing) $ do
                renderOnTop canvas $ drawB fromCan (x*sz) (y*sz)
                sequence_ [renderOnTop canvas $
                  drawB toCan (x1*sz) (y1*sz) | (x1, y1) <- movesFrom i game]
              loop game { selection = sel }
            else if i `elem` movesFrom (fromJust sel0) game then
              loop game { anim = Just (0, (fromJust sel0, i)) }
            else
              loop game { selection = Nothing }
        Ke 113 -> drawGame initGame >> loop initGame
        _ -> loop game

    else let Just (frame, m@((x0, y0), (x1, y1))) = anim game in
      if frame == 8 then let game1 = move game m in do
        drawGame game1
        if state game1 == Play && player game1 == -1 then do
          wait 1  -- Delay for redraw.
          ms <- shuffleIO $ nextMoves game1
          loop game1 { anim = Just (0, best game1 ms) }
        else
          loop game1
      else let f x0 x1 frame = x0 * sz + (x1 - x0) * sz * frame `div` 8 in do
        drawGame game { board = board game // [((x0, y0), 0)] }
        renderPiece canvas (player game) (f x0 x1 frame, f y0 y1 frame)
        void $ setTimer (Once 20) $ loop game { anim = Just (frame + 1, m) }

  concurrent $ forkIO $ drawGame initGame >> loop initGame
\end{code}
