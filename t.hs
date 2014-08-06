import Control.Concurrent.MVar
import Data.Ix
import Data.Array
import Data.Maybe
import Control.Monad
import Haste
import Haste.Graphics.Canvas

sz = 64
bd = 4
bnds = ((0,0), (2,2))

initBoard :: Array (Int, Int) Char
initBoard = array bnds [(i, '.') | i <- range bnds]

data Event = MouseDown Int Int

data Status = Draw | Won | Play deriving Eq

data Game = Game { board :: Array (Int, Int) Char
                 , status :: Status
                 , player :: Char
                 }

intRect x y w h = Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

oblong x y w h = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x + w), fromIntegral (y + h))

nextPlayer 'X' = 'O'
nextPlayer 'O' = 'X'

main = do
  xo <- loadBitmap "xo.png"
  Just canvasElem <- elemById "canvas"
  Just canvas <- getCanvas canvasElem
  Just stateElem <- elemById "state"
  evq <- newEmptyMVar
  putMVar evq []
  canvasElem  `onEvent` OnMouseDown $ \_button (x, y) -> do
    q <- takeMVar evq
    putMVar evq (q ++ [MouseDown x y])
  seedVar <- newEmptyMVar
  seed <- newSeed
  putMVar seedVar seed
  let
    randomRIO range = do
      seed <- takeMVar seedVar
      let (a, seed1) = randomR range seed in do
        putMVar seedVar seed1
        return a

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

    statusCheck game@(Game board Play player) =
      if or $      and [board!(x,x) == player | x <- [0..2]]
            :      and [board!(x,2 - x) == player | x <- [0..2]]
            :  map and [[board!(x,y) == player | x <- [0..2]] | y <- [0..2]]
            ++ map and [[board!(y,x) == player | x <- [0..2]] | y <- [0..2]] then
        Game board Won player
      else if and [board!(x,y) /= '.' | x <- [0..2], y <- [0..2]] then
        Game board Draw player
      else
        Game board Play (nextPlayer player)

    handle game@(Game board _ player) (MouseDown x y) = let
      j = (x `div` sz, y `div` sz)
      in if inRange bnds j && board!j == '.' then statusCheck $ Game (board // [(j, player)]) Play player else game

    aiMove :: Game -> IO Game
    aiMove game@(Game board Play 'O') = let moves = [(x, y) | (x, y) <- range bnds, board!(x, y) == '.'] in do
      n <- randomRIO (0, length moves - 1)
      return $ statusCheck $ Game (board // [(moves!!n, 'O')]) Play 'O'
    aiMove game = return game

    loop game@(Game board status player) = do
      render canvas $ sequence_ [sq (board!i) x y | i@(x, y) <- range bnds] 
      setProp stateElem "innerHTML" $ case status of
        Won -> player : " wins"
        Draw -> "Draw"
        Play -> player : " to move"
      q <- takeMVar evq
      putMVar evq []
      let
        game1 = if null q || status /= Play then game else handle game (head q)
        in do
          game2 <- aiMove game1
          setTimeout 10 $ loop game2

    in loop $ Game initBoard Play 'X'
