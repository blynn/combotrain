-- We use "setTimeout 1" to refresh the screen. Is there a cleaner way?
import Control.Concurrent.MVar
import Data.Array
import Data.Function
import Data.List
import Data.Tree
import Control.Applicative
import Control.Monad
import Haste
import Haste.Graphics.Canvas

sz = 64; bd = 4; bnds = ((0,0), (2,2))
moveRandomly = False

data Status = Draw | Won | Play deriving Eq
data Game = Game (Array (Int, Int) Int) Status Int

initGame = Game (listArray bnds $ repeat 0) Play (-1)

goals = [join (,) <$> [0..2], (,) <*> (2-) <$> [0..2]]  -- Diagonals.
        ++ ((<$> [0..2]) .      (,) <$> [0..2])  -- Rows and columns.
        ++ ((<$> [0..2]) . flip (,) <$> [0..2])

move (Game board0 Play player) i
  | or $ and . ((== player) . (board!) <$>) <$> goals = Game board Won  player
  | Nothing == find (== 0) (elems board)              = Game board Draw player
  | otherwise                            = Game board Play $ -player
  where board = board0 // [(i, player)]

nextMoves game@(Game board status _) = (game, case status of
  Play -> [move game i | i <- range bnds, board!i == 0]
  _    -> [])

gameTree = unfoldTree nextMoves

score (Game _ Won n) = n
score _              = 0

maximize (Node leaf [])   = leaf
maximize (Node _    kids) = maximum (map minimize kids)

minimize (Node leaf [])   = leaf
minimize (Node _    kids) = minimum (map maximize kids)

best xs = snd $ maximumBy (compare `on` fst) $
  map (\x -> (minimize $ fmap score $ gameTree x, x)) xs

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

intRect x y w h = Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

oblong x y w h = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x + w), fromIntegral (y + h))

main = withElems ["body", "canvas", "message", "noab"] $
                \[body, cElem, message, noab] -> do
  xo <- loadBitmap "xo.png"
  Just canvas <- getCanvas cElem
  seedVar <- newSeed >>= newMVar
  gameVar <- newMVar initGame
  let
    randomRIO r = do
      (a, seed1) <- randomR r <$> takeMVar seedVar
      putMVar seedVar seed1
      return a

    shuffleIO [] = return []
    shuffleIO xs = randomRIO (0, length xs - 1) >>= \n ->
      let (a, b:bs) = splitAt n xs in (b:) <$> shuffleIO (a ++ bs)

    sq ((x, y), p) = do
      -- Draw borders.
      when (x /= 0) $ oblong (x * sz)           (y * sz) bd sz
      when (x /= 2) $ oblong (x * sz + sz - bd) (y * sz) bd sz
      when (y /= 0) $ oblong (x * sz)           (y * sz) sz bd
      when (y /= 2) $ oblong (x * sz) (y * sz + sz - bd) sz bd
      -- Draw nought or cross when present.
      when (p == -1) $ drawClipped xo (fromIntegral (x * sz), fromIntegral (y * sz)) (intRect 0 0 sz sz)
      when (p ==  1) $ drawClipped xo (fromIntegral (x * sz), fromIntegral (y * sz)) (intRect sz 0 sz sz)

    aiMove game = do
      shuffledMoves <- shuffleIO $ snd $ nextMoves game
      disableAB <- getProp noab "checked"
      return $ if moveRandomly then head shuffledMoves else
        if disableAB == "true" then best shuffledMoves else bestAB shuffledMoves

    go game = swapMVar gameVar game >> update

    update = do
      game@(Game board status player) <- readMVar gameVar
      render canvas $ mapM_ sq $ assocs board
      setProp message "innerHTML" $ case status of
        Won  -> ("X.O"!!(player + 1)) : " wins"
        Draw -> "Draw"
        Play -> if player == -1 then "X to move" else "Thinking..."
      when (player == 1 && status == Play) $ setTimeout 1 $ aiMove game >>= go

  void $ cElem `onEvent` OnMouseDown $ \_ (x, y) -> do
    game@(Game board status player) <- readMVar gameVar
    let i = (x `div` sz, y `div` sz) in when (status == Play && player == -1
       && inRange bnds i && board!i == 0) $ go $ move game i

  void $ body `onEvent` OnKeyDown $ \k -> when (k == 113) $ go initGame

  update
