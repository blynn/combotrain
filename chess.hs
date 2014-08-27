import Control.Monad
import Data.Array
import Data.List
import Data.Maybe
import Data.Tree
import Haste
import qualified Haste.Concurrent as H
import Haste.Graphics.Canvas

bnds = ((0,0), (7,7)); sz = 40

onBoard = inRange bnds

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Side = White | Black deriving (Eq, Show)
data Square = Square Side Piece deriving (Eq, Show)
data Event = KeyDown Int | Click Int Int
data State = Draw | Won | Play deriving Eq
data Game = Game { board :: Array (Int, Int) (Maybe Square)
                 , state :: State
                 , player :: Side
                 , selection :: Maybe (Int, Int)
                 , anim :: Maybe (Int, ((Int, Int), (Int, Int)))
                 , canCastle :: [(Int, Int)]
                 , enPassant :: Maybe (Side, (Int, Int))
                 , lastMove :: ((Int, Int), (Int, Int))
                 , promoChoice :: Piece -- TODO: Should be part of move.
                 }

side (Just (Square s _)) = s

piece (Just (Square _ p)) = p

initBoard = let
  order = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
  f (_, 1) = Just $ Square Black Pawn
  f (x, 0) = Just $ Square Black $ order!!x
  f (_, 6) = Just $ Square White Pawn
  f (x, 7) = Just $ Square White $ order!!x
  f _ = Nothing
  in array bnds [(i, f i) | i <- range bnds]

initGame = Game initBoard Play White Nothing Nothing [(x,y) | x <- [0,4,7], y <- [0,7]] Nothing undefined Queen

worth Pawn   = 100
worth Knight = 300
worth Bishop = 350
worth Rook   = 500
worth Queen  = 1000
worth King   = 0

toPiece "Knight" = Knight
toPiece "Bishop" = Bishop
toPiece "Rook"   = Rook
toPiece _        = Queen

score game
  | state game == Won  = if player game == Black then 2^16 else -2^16
  | state game == Draw = 0
  | otherwise          = let b = board game in sum [(if side (b!i) == White then -1 else 1) * worth (piece (b!i)) | i <- range bnds, b!i /= Nothing]

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

best game ms = lastMove $ fst $ maximize $ prune 3 $
  Node game (map (gameTree . move game) ms)

gameTree = unfoldTree (\x -> (x, nextNodes x))

nextNodes game = if state game == Play then [move game m | m <- legalMoves game] else []

prune 0 (Node a _) = Node a []
prune n (Node a kids) = Node a $ map (prune (n - 1)) kids

box :: Int -> Int -> Int -> Int -> Picture ()
box x y dx dy = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x+dx), fromIntegral (y+dy))

sqColor False = RGB 191 191 191
sqColor True  = RGB 255 255 255

drawB pic x y = draw pic (fromIntegral x, fromIntegral y)

nextPlayer White = Black
nextPlayer Black = White

dirPlayer White = -1
dirPlayer Black = 1

isCheck p game = let
  b = board game
  k = head [i | i <- range bnds, (b!i) == Just (Square p King)]
  in or [k `elem` movesFrom i game | i <- range bnds, (b!i) /= Nothing, side (b!i) /= p]

legalMovesFrom i@(x, y) game = let
  b = board game
  cc = canCastle game
  p = player game
  in (filter (\m -> not . isCheck p $ movePrecheck game (i, m)) $ movesFrom i game) ++
    if i `elem` cc && x == 4 then
      (if (0, y) `elem` cc && and [b!(x1, y) == Nothing | x1 <- [1..3]] && and [not $ isCheck p $ movePrecheck game (i, (x1, y)) | x1 <- [2, 3]] then [(2, y)] else [])
      ++
      (if (7, y) `elem` cc && and [b!(x1, y) == Nothing | x1 <- [5, 6]] && and [not $ isCheck p $ movePrecheck game (i, (x1, y)) | x1 <- [5, 6]] then [(6, y)] else [])
    else []

legalMoves game = let b = board game in [(i, m) | i <- range bnds, b!i /= Nothing, side (b!i) == (player game), m <- legalMovesFrom i game]

-- All moves except castling. 
movesFrom i@(x, y) game = let
  b = board game
  ep = enPassant game
  p = side (b!i)
  cap j = onBoard j && b!j /= Nothing && side (b!j) /= p
  blank j = onBoard j && b!j == Nothing
  blankCap j = onBoard j && (b!j == Nothing || side (b!j) /= p)
  pawnStart = (p == White && y == 6) || (p == Black && y == 1)
  scan dx dy = unfoldr (\(x', y', cont) -> if cont && blankCap (x + x', y + y') then Just ((x + x', y + y'), (x' + dx, y' + dy, blank (x + x', y + y'))) else Nothing) (dx, dy, True)
  in case piece (b!i) of
    Pawn   -> let i1 = (x, y + dirPlayer p) in (if blank i1 then i1 : (let i2 = (x, y + 2 * dirPlayer p) in if pawnStart && blank i2 then [i2] else []) else [])
      ++ [j | dx <- [-1, 1], let j = (x + dx, y + dirPlayer p), cap j || (ep /= Nothing && let Just (es, ej) = ep in j == ej && es /= p)]
    Knight -> [i1 | a <- [-1, 1], b <- [-1, 1], f <- [\a b -> (2*a, b), \a b -> (a, 2*b)], let (dx, dy) = f a b, let i1 = (x+dx, y+dy), blankCap i1]
    Bishop -> concat [scan dx dy | dx <- [-1, 1], dy <- [-1, 1]]
    Rook   -> concat [scan dx dy | a <- [-1, 1], f <- [id, \(x, y) -> (y, x)], let (dx, dy) = f (a, 0)]
    Queen  -> concat [scan dx dy | dx <- [-1..1], dy <- [-1..1]]
    King   -> [i1 | dx <- [-1..1], dy <- [-1..1], let i1 = (x+dx, y+dy), blankCap i1]

movePrecheck game m@(i0@(x0, y0), i1@(x1, y1)) = let
  b = board game
  p = player game
  ep = enPassant game
  promoCheck a@((x, y), Just (Square s Pawn)) = if (s == Black && y == 7) || (s == White && y == 0) then ((x, y), Just (Square s (if s == Black then Queen else promoChoice game))) else a
  promoCheck a = a
  castleCheck xs =
   if piece (b!i0) == King then
      if x0 - x1 == 2 then ((0, y0), Nothing) : ((3, y0), b!(0, y0)) : xs
      else if x0 - x1 == -2 then ((7, y0), Nothing) : ((5, y0), b!(7, y0)) : xs
      else xs
    else if piece (b!i0) == Pawn && ep /= Nothing && i1 == snd (fromJust $ enPassant game) then
      ((x1, y1 - dirPlayer p), Nothing) : xs
    else xs
  in game { board = b // castleCheck [(i0, Nothing), promoCheck (i1, b!i0)]
          , state = Play
          , player = nextPlayer p
          , selection = Nothing
          , anim = Nothing
          , canCastle = delete i0 (canCastle game)
          , enPassant = if piece (b!i0) == Pawn && y0 + dirPlayer p /= y1 then
              Just (p, (x0, y0 + dirPlayer p))
            else Nothing
          , lastMove = m
          }

move game m = let game1 = movePrecheck game m in
  if legalMoves game1 == [] then
    game1 { state = if isCheck (player game1) game1 then Won else Draw
          , player = player game }
  else
    game1

main = withElems ["body", "canvas", "message", "promo"] $ \[body, canvasE, msg, promoSel] -> do
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

  ev <- H.newEmptyMVar
  canvasE  `onEvent` OnMouseDown $ \_button (x, y) -> H.concurrent $ H.putMVar ev $ Click x y
  body `onEvent` OnKeyDown $ \k -> H.concurrent $ H.putMVar ev $ KeyDown k

  seed <- newSeed
  seedV <- H.newMVar seed

  let
    renderPiece c sq (x,y) = renderOnTop c $ translate (fromIntegral (x + if side sq == Black then 40 - 5 else 5), fromIntegral (y + 20)) $
     (if side sq == Black then rotate pi else id) $ text (0, 0) (show $ piece sq)

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

    drawGame game = let b = board game in do
      sequence_ $ (render buf $ draw boardCan (0, 0)) : [renderPiece buf sq (x*sz, y*sz) | i@(x, y) <- range bnds, let sq = b!i, sq /= Nothing]
      render canvas $ draw buf (0, 0)
      setProp msg "innerHTML" $ show (player game) ++ case state game of
        Play -> " to move"
        Won -> " wins"
        Draw -> " draws"

    loop game = let b = board game in if anim game == Nothing then do
      e <- H.takeMVar ev
      case e of
        Click bx by -> when (state game == Play) $ let
          sel0 = selection game
          i@(x, y) = (div bx sz, div by sz)
          sel = if b!i /= Nothing && side (b!i) == player game then Just i else Nothing
          in when (inRange bnds i) $ do
            render canvas $ draw buf (0, 0)
            if sel0 == Nothing then do
              unless (sel == Nothing) $ do
                renderOnTop canvas $ drawB fromCan (x*sz) (y*sz)
                sequence_ [renderOnTop canvas $
                  drawB toCan (x1*sz) (y1*sz) | (x1, y1) <- legalMovesFrom i game]
              loop game { selection = sel }
            else if i `elem` legalMovesFrom (fromJust sel0) game then do
              s <- getProp promoSel "value"
              loop game { anim = Just (0, (fromJust sel0, i)), promoChoice = toPiece s }
            else
              loop game { selection = Nothing }

        KeyDown 113 -> do
          drawGame initGame
          loop initGame

        _ -> loop game

      else let Just (frame, m@(from@(x0, y0), (x1, y1))) = anim game in
        if frame == 8 then let game1 = move game m in do
          drawGame game1
          -- Delay so canvas has a chance to update.
          if state game1 == Play && player game1 == Black then
            setTimeout 20 $ H.concurrent $ do
            ms <- shuffleIO $ legalMoves game1
            loop game1 { anim = Just (0, best game1 ms) }
          else
            loop game1

        else let f x0 x1 frame = x0 * sz + (x1 - x0) * sz * frame `div` 8 in do
          drawGame game { board = b // [(from, Nothing)] }
          renderPiece canvas (b!from) (f x0 x1 frame, f y0 y1 frame)
          setTimeout 20 $ H.concurrent $ loop game { anim = Just (frame + 1, m) }

    game = initGame in do
      drawGame game
      H.concurrent $ H.forkIO $ loop game
