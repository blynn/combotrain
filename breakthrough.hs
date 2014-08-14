import Control.Monad
import Data.Array
import Data.Maybe
import Haste
import qualified Haste.Concurrent as HC
import Haste.Graphics.Canvas

bnds = ((0,0), (7,7)); sz = 40

data Event = KeyDown Int | Click Int Int
data State = Won | Play deriving Eq
data Game = Game { board :: Array (Int, Int) Int
                 , state :: State
                 , player :: Int
                 , selection :: Maybe (Int, Int)
                 }

initBoard = let
  initRow y | y <= 1 = -1
            | y >= 6 = 1
            | True   = 0
  in array bnds [(i, initRow y) | i@(x,y) <- range bnds]

initGame = Game initBoard Play 1 Nothing

box :: Int -> Int -> Int -> Int -> Picture ()  -- Why is this needed?
box x y dx dy = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x+dx), fromIntegral (y+dy))

sqColor False = RGB 191 191 191
sqColor True  = RGB 255 255 255

drawB pic x y = draw pic (fromIntegral x, fromIntegral y)

playerName   1  = "White"
playerName (-1) = "Black"

movesFrom (x, y) game =
  let
    b = board game
    p = player game
  in [i1 | dx <- [-1, 0, 1], let i1 = (x + dx, y - p), inRange bnds i1, b!i1 /= p, dx /= 0 || b!i1 == 0]

move (Game board state player _) i0 i1@(_, y1) = let
  nextBoard = board // [(i0, 0), (i1, player)]
  nextState = if (player == 1 && y1 == 0) || (player == -1 && y1 == 7) then Won else Play
  in Game nextBoard nextState (if nextState == Won then player else -player) Nothing

main = withElems ["body", "canvas", "message"] $ \[body, canvasE, msg] -> do
  Just canvas <- getCanvas canvasE
  Just whitePiece <- createCanvas sz sz
  renderOnTop whitePiece $ color (RGB 255 255 255) $ fill $ circle (20, 20) 10
  renderOnTop whitePiece $ color (RGB 0 0 0) $ stroke $ circle (20, 20) 11
  Just blackPiece <- createCanvas sz sz
  renderOnTop blackPiece $ color (RGB 0 0 0) $ fill $ circle (20, 20) 11

  Just fromCan <- createCanvas sz sz
  render fromCan $ color (RGB 127 15 15) $ sequence_
    [ box 0 0 5 40, box 0 0 40 5, box 35 0 40 40, box 0 35 40 40 ]
  Just toCan <- createCanvas sz sz
  render toCan $ color (RGBA 0 191 0 0.3) $ box 0 0 sz sz

  Just boardCan <- createCanvas 320 320
  sequence_ $ [renderOnTop boardCan $ color (sqColor (mod (x + y) 2 == 0)) $ box (x*sz) (y*sz) sz sz | (x, y) <- range bnds]
  Just buf <- createCanvas 320 320
  Just selMask <- createCanvas 320 320

  ev <- HC.newEmptyMVar
  canvasE  `onEvent` OnMouseDown $ \_button (x, y) -> HC.concurrent $ HC.putMVar ev $ Click x y
  body `onEvent` OnKeyDown $ \k -> HC.concurrent $ HC.putMVar ev $ KeyDown k

  let
    drawGame game@(Game board state player _) = do
      sequence_ $ (render buf $ draw boardCan (0, 0)) : [renderOnTop buf $ draw (if p == 1 then whitePiece else blackPiece) (fromIntegral (x*sz), fromIntegral (y*sz)) | i@(x, y) <- range bnds, let p = board!i, p /= 0]
      render canvas $ draw buf (0, 0)
      setProp msg "innerHTML" $ playerName player ++ case state of
        Play -> " to move"
        Won -> " wins"

    loop game@(Game board _ player sel0) = do
      e <- HC.takeMVar ev
      case e of
        Click bx by -> let
          i@(x, y) = (div bx sz, div by sz)
          sel = if board!i == player then Just i else Nothing
          in when (inRange bnds i) $ do
            render canvas $ draw buf (0, 0)
            if sel0 == Nothing then do
              case sel of
                Nothing -> render selMask $ return ()
                Just _ -> do
                  render selMask $ drawB fromCan (x*sz) (y*sz)
                  let y1 = y - player in sequence_ [renderOnTop selMask $
                    drawB toCan (x1*sz) (y1*sz) | (x1, y1) <- movesFrom i game]

              renderOnTop canvas $ draw selMask (0, 0)
              loop $ game { selection = sel }
            else let
              ms = movesFrom (fromJust sel0) game
              in if (i `elem` ms) then
                let game1 = move game (fromJust sel0) i in do
                  drawGame game1
                  loop game1
              else
                loop game { selection = Nothing }

        KeyDown 113 -> do
          drawGame initGame
          loop initGame

        _ -> loop game

    game = initGame in do
      drawGame game
      HC.concurrent $ HC.fork $ loop game
