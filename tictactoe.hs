import Control.Concurrent.MVar
import Data.Ix
import Data.Array
import Data.Maybe
import Data.Tree
import Control.Monad
import Haste
import Haste.Graphics.Canvas

sz = 64
bd = 4
bnds = ((0,0), (2,2))
moveRandomly = False

data Event = KeyDown Int | MouseDown Int Int

data Status = Draw | Won | Play deriving Eq

data Game = Game { board :: Array (Int, Int) Char
                 , status :: Status
                 , player :: Char
                 }

initGame = Game (array bnds [(i, '.') | i <- range bnds]) Play 'X'

intRect x y w h = Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

oblong x y w h = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x + w), fromIntegral (y + h))

nextPlayer 'X' = 'O'
nextPlayer 'O' = 'X'

move (Game board0 Play player) i = let board = board0 // [(i, player)] in
  if or $      and [board!(x,x) == player | x <- [0..2]]
        :      and [board!(x,2 - x) == player | x <- [0..2]]
        :  map and [[board!(x,y) == player | x <- [0..2]] | y <- [0..2]]
        ++ map and [[board!(y,x) == player | x <- [0..2]] | y <- [0..2]] then
    Game board Won player
  else if and [board!i /= '.' | i <- range bnds] then
    Game board Draw player
  else
    Game board Play (nextPlayer player)

nextMoves game@(Game board status player) = (game, case status of
  Play -> [move game i | i <- range bnds, board!i == '.']
  _ -> [])

gameTree = unfoldTree nextMoves 

score (Game _ Won 'X') = -1
score (Game _ Won 'O') = 1
score _ = 0

maximize (Node leaf []) = score leaf
maximize (Node _ kids) = maximum (map minimize kids)

minimize (Node leaf []) = score leaf
minimize (Node _ kids) = minimum (map maximize kids)

best (x:xs) = let
  f [] n bestYet = bestYet
  f (x:xs) n bestYet = let n' = minimize ( gameTree x ) in if n' > n then f xs n' x else f xs n bestYet
  in f xs (minimize $ gameTree x) x

handle game@(Game board status player) (MouseDown x y) = let
  j = (x `div` sz, y `div` sz)
  in if status == Play && inRange bnds j && board!j == '.' then move game j else game

handle game (KeyDown sym) = case sym of
  113 -> initGame
  _ -> game

main = withElems ["body", "canvas", "message"] $ \[body, canvasElem, message] -> do
  xo <- loadBitmap "xo.png"
  Just canvas <- getCanvas canvasElem
  evq <- newEmptyMVar
  putMVar evq []
  canvasElem  `onEvent` OnMouseDown $ \_button (x, y) -> do
    q <- takeMVar evq
    putMVar evq (q ++ [MouseDown x y])
  body `onEvent` OnKeyDown $ \_k -> do
    q <- takeMVar evq
    putMVar evq (q ++ [KeyDown _k])
  seedVar <- newEmptyMVar
  seed <- newSeed
  putMVar seedVar seed
  let
    randomRIO range = do
      seed <- takeMVar seedVar
      let (a, seed1) = randomR range seed in do
        putMVar seedVar seed1
        return a

    shuffleIO :: [a] -> IO [a]
    shuffleIO [] = return []
    shuffleIO xs = do
      n <- randomRIO (0, length xs - 1)
      let (a, b:bs) = splitAt n xs in do 
        ys <- shuffleIO (a ++ bs)
        return (b:ys)

    sq :: Char -> Int -> Int -> Picture ()
    sq c x y = do
      -- Draw borders.
      when (x /= 0) $ oblong (x * sz)           (y * sz) bd sz
      when (x /= 2) $ oblong (x * sz + sz - bd) (y * sz) bd sz
      when (y /= 0) $ oblong (x * sz)           (y * sz) sz bd
      when (y /= 2) $ oblong (x * sz) (y * sz + sz - bd) sz bd
      -- Draw nought or cross when present.
      when (c == 'X') $ drawClipped xo (fromIntegral (x * sz), fromIntegral (y * sz)) (intRect 0 0 sz sz)
      when (c == 'O') $ drawClipped xo (fromIntegral (x * sz), fromIntegral (y * sz)) (intRect sz 0 sz sz)

    aiMove :: Game -> IO Game
    aiMove game@(Game _ Play 'O') = let moves = snd $ nextMoves game in
      if moveRandomly then do
        n <- randomRIO (0, length moves - 1)
        return $ moves!!n
      else do
        shuffledMoves <- shuffleIO moves
        return $ best shuffledMoves
    aiMove game = return game

    loop game = do
      q <- swapMVar evq []
      let game1@(Game board status player) = if null q then game else handle game (head q) in do
        render canvas $ sequence_ [sq (board!i) x y | i@(x, y) <- range bnds] 
        setProp message "innerHTML" $ case status of
          Won -> player : " wins"
          Draw -> "Draw"
          Play -> if player == 'X' then "X to move" else "Thinking..."
        game2 <- aiMove game1
        setTimeout 10 $ loop game2

    in loop $ initGame
