import Control.Monad
import Data.Array
import Data.Maybe
import Data.Tree
import Haste
import qualified Haste.Concurrent as H
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

score (Game _ Won player _) = -player * 1024
score (Game board Play _ _) = -1 * sum [board!i | i <- range bnds]

maximize' :: Tree Int -> [Int]
maximize' (Node leaf []) = [leaf]
maximize' (Node _ kids) = let
  mapmin (ns:nss) = (minimum ns : omit (minimum ns) nss)
  omit pot [] = []
  omit pot (ns:nss) | minleq ns pot = omit pot nss
                    | otherwise     = (minimum ns : omit (minimum ns) nss)
  minleq [] pot = False
  minleq (n:ns) pot | n <= pot = True
                    | True     = minleq ns pot
  in mapmin (map minimize' kids)
maximize = maximum . maximize'

minimize' :: Tree Int -> [Int]
minimize' (Node leaf []) = [leaf]
minimize' (Node _ kids) = let
  mapmax (ns:nss) = (maximum ns : omit (maximum ns) nss)
  omit pot [] = []
  omit pot (ns:nss) | maxgeq ns pot = omit pot nss
                    | otherwise     = (maximum ns : omit (maximum ns) nss)
  maxgeq [] pot = False
  maxgeq (n:ns) pot | n >= pot = True
                    | True     = maxgeq ns pot
  in mapmax (map maximize' kids)
minimize = minimum . minimize'

gameTree = unfoldTree nextStates 

nextMoves game@(Game board Play player _) = [move game i dst | i <- range bnds, board!i == player, dst <- movesFrom i game]
nextMoves game@(Game _ Won _ _) = []

nextStates game = (game, [g | g <- nextMoves game])

prune 0 (Node a _) = Node a []
prune n (Node a kids) = Node a $ map (prune (n - 1)) kids

best (x:xs) = let
  sc x = minimize $ fmap score $ prune 3 $ gameTree x
  f [] n bestYet = bestYet
  f (x:xs) n bestYet = let n' = sc x in if n' > n then f xs n' x else f xs n bestYet
  in f xs (sc x) x

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

  ev <- H.newEmptyMVar
  canvasE  `onEvent` OnMouseDown $ \_button (x, y) -> H.concurrent $ H.putMVar ev $ Click x y
  body `onEvent` OnKeyDown $ \k -> H.concurrent $ H.putMVar ev $ KeyDown k

  seed <- newSeed
  seedV <- H.newMVar seed

  let
    randomRIO range = do
      seed <- H.takeMVar seedV
      let (r, seed1) = randomR range seed in do
        H.putMVar seedV seed1
        return r

    shuffleIO [] = return []
    shuffleIO xs = do
      n <- randomRIO (0, length xs - 1)
      let (a, b:bs) = splitAt n xs in do 
        ys <- shuffleIO (a ++ bs)
        return (b:ys)

    drawGame game@(Game board state player _) = do
      sequence_ $ (render buf $ draw boardCan (0, 0)) : [renderOnTop buf $ draw (if p == 1 then whitePiece else blackPiece) (fromIntegral (x*sz), fromIntegral (y*sz)) | i@(x, y) <- range bnds, let p = board!i, p /= 0]
      render canvas $ draw buf (0, 0)
      setProp msg "innerHTML" $ playerName player ++ case state of
        Play -> " to move"
        Won -> " wins"

    loop game@(Game board _ player sel0) = do
      e <- H.takeMVar ev
      case e of
        Click bx by -> when (state game == Play) $ let
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
            else
              if i `elem` movesFrom (fromJust sel0) game then
                let game1 = move game (fromJust sel0) i in do
                  drawGame game1
                  -- Delay so canvas has a chance to update.
                  if state game1 == Play then
                    setTimeout 20 $ H.concurrent $ do
                    ms <- shuffleIO $ nextMoves game1
                    let game2 = best ms in do
                      drawGame game2
                      loop game2
                  else
                    loop game1
              else
                loop game { selection = Nothing }

        KeyDown 113 -> do
          drawGame initGame
          loop initGame

        _ -> loop game

    game = initGame in do
      drawGame game
      H.concurrent $ H.forkIO $ loop game
