import Control.Concurrent.MVar
import Data.Ix
import Data.Array
import Data.Maybe
import Data.Tree
import Control.Monad
import Haste
import Haste.Graphics.Canvas
import System.IO.Unsafe

sz = 64
bd = 4
bnds = ((0,0), (2,2))

data Event = KeyDown Int | MouseDown Int Int

data Status = Draw | Won | Play deriving (Eq, Show)

data Game = Game { board :: Array (Int, Int) Char
                 , status :: Status
                 , player :: Char
                 } deriving Show

initGame = Game (array bnds [(i, '.') | i <- range bnds]) Play 'X'

intRect x y w h = Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

oblong x y w h = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x + w), fromIntegral (y + h))

nextPlayer 'X' = 'O'
nextPlayer 'O' = 'X'

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

nextMoves game@(Game board status player) = (game, case status of
  Draw -> []
  Won -> []
  Play -> [statusCheck $ Game (board // [(i, player)]) Play player | i@(x, y) <- range bnds, board!i == '.'])

gameTree = unfoldTree nextMoves 

maximize :: Tree Game -> Int
maximize (Node game@(Game _ status player) []) = case status of
  Draw -> 0
  Won -> if player == 'X' then -1 else 1
maximize (Node _ sub) = maximum (map minimize sub)

minimize :: Tree Game -> Int
minimize (Node game@(Game _ status player) []) = case status of
  Draw -> 0
  Won -> if player == 'X' then -1 else 1
minimize (Node _ sub) = minimum (map maximize sub)

best :: [Game] -> Int -> Game -> Game
best [] n bestYet = bestYet
best (x:xs) n bestYet = let n' = minimize ( gameTree x ) in if n' > n then best xs n' x else best xs n bestYet

main = do
  xo <- loadBitmap "xo.png"
  Just canvasElem <- elemById "canvas"
  Just canvas <- getCanvas canvasElem
  Just body <- elemById "body"
  Just stateElem <- elemById "state"
  Just debugElem <- elemById "debug"
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

    handle game@(Game board status player) (MouseDown x y) = let
      j = (x `div` sz, y `div` sz)
      in if status == Play && inRange bnds j && board!j == '.' then statusCheck $ Game (board // [(j, player)]) Play player else game

    handle game (KeyDown sym) = case sym of
      68 -> unsafePerformIO $ do
        setProp debugElem "innerHTML" $ show $ map (map (\x -> (x, minimize (gameTree x))))$ take 3 $ levels $ gameTree game
        return game
      113 -> initGame
      _ -> game

    aiMove :: Game -> IO Game
    aiMove game@(Game _ Play 'O') = let moves = snd $ nextMoves game in return $ best (tail moves) (minimize $ gameTree $ head moves) (head moves)
    {-
    aiMove game@(Game board Play 'O') = let moves = [(x, y) | (x, y) <- range bnds, board!(x, y) == '.'] in do
      n <- randomRIO (0, length moves - 1)
      return $ statusCheck $ Game (board // [(moves!!n, 'O')]) Play 'O'
      -}
    aiMove game = return game

    loop game = do
      q <- takeMVar evq
      putMVar evq []
      let game1@(Game board status player) = if null q then game else handle game (head q) in do
        render canvas $ sequence_ [sq (board!i) x y | i@(x, y) <- range bnds] 
        setProp stateElem "innerHTML" $ case status of
          Won -> player : " wins"
          Draw -> "Draw"
          Play -> player : " to move"
        game2 <- aiMove game1
        setTimeout 10 $ loop game2

    in loop $ initGame
