= Chess =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<div id="body">
<script src="chess.js"></script>
<canvas id="canvas" style="border:1px solid black;display:block;margin:auto;" width="320" height="320"></canvas>
<div style="text-align:center;">
<div id="message">
</div>
<p>
Promote your next pawn to:
<select id="promo">
  <option value="Queen">Queen</option>
  <option value="Rook">Rook</option>
  <option value="Bishop">Bishop</option>
  <option value="Knight">Knight</option>
</select>
</p>
</div>
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

We use a spurious `traceShow` to work around a Haste bug.

\begin{code}
{-# LANGUAGE CPP #-}
import Control.Monad
import Data.Array
import Data.IORef
import Data.List
import Data.Maybe
import Data.Tree
#ifdef __HASTE__
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import System.Random
import Debug.Trace
#endif

bnds = ((0,0), (7,7)); sz = 40

onBoard = inRange bnds

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Side = White | Black deriving (Eq, Show)
data Square = Square Side Piece deriving (Eq, Show)
data Event = EKeyDown Int | EClick Int Int
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
worth Queen  = 900
worth King   = 0

toPiece "Knight" = Knight
toPiece "Bishop" = Bishop
toPiece "Rook"   = Rook
toPiece _        = Queen

score game
  | state game == Won  = if player game == Black then 2^16 else -2^16
  | state game == Draw = 0
  | otherwise          = let b = board game in sum [(if side (b!i) == White then -1 else 1) * worth (piece (b!i)) | i <- range bnds, b!i /= Nothing]

omitWith op (ms:mss) = m : omit m mss where
  oppest = foldr1 (\a b -> if op a b then a else b)
  m = oppest ms
  omit _   [] = []
  omit pot (ns:nss) | any (`op` pot) ns = omit pot nss
                    | otherwise = n : omit n nss
                    where n = oppest ns

maximize' :: Tree Game -> [Int]
maximize' (Node leaf [])   = [score leaf]
maximize' (Node _    kids) = omitWith (<=) $ minimize' <$> kids

minimize' :: Tree Game -> [Int]
minimize' (Node leaf [])   = [score leaf]
minimize' (Node _    kids) = omitWith (>=) $ maximize' <$> kids

best game ms = snd $ foldr1 go $ zip (minimum . minimize' . prune 2 . gameTree <$> gs) ms
  where
  gs = move game <$> ms
  go a b = if fst a > fst b then a else b

gameTree = unfoldTree (\x -> (x, nextNodes x))

nextNodes game = if state game == Play then [move game m | m <- legalMoves game] else []

prune 0 (Node a _) = Node a []
prune n (Node a kids) = Node a $ map (prune (n - 1)) kids

nextPlayer White = Black
nextPlayer Black = White

dirPlayer White = -1
dirPlayer Black = 1

-- All moves except castling.
#ifdef __HASTE__
movesFrom i@(x, y) game = traceShow 0 $ case piece (b!i) of
#else
movesFrom i@(x, y) game = case piece (b!i) of
#endif
  Pawn   -> let i1 = (x, y + dirPlayer p) in (if blank i1 then i1 : (let i2 = (x, y + 2 * dirPlayer p) in if pawnStart && blank i2 then [i2] else []) else [])
    ++ [j | dx <- [-1, 1], let j = (x + dx, y + dirPlayer p), cap j || (ep /= Nothing && let Just (es, ej) = ep in j == ej && es /= p)]
  Knight -> [i1 | a <- [-1, 1], b <- [-1, 1], (dx, dy) <- [(2*a, b), (a, 2*b)], let i1 = (x+dx, y+dy), blankCap i1]
  Bishop -> concat [scan dx dy | dx <- [-1, 1], dy <- [-1, 1]]
  Rook   -> concat [scan dx dy | a <- [-1, 1], (dx, dy) <- [(a, 0), (0, a)]]
  Queen  -> concat [scan dx dy | dx <- [-1..1], dy <- [-1..1]]
  King   -> [i1 | dx <- [-1..1], dy <- [-1..1], let i1 = (x+dx, y+dy), blankCap i1]
  where
  b = board game
  ep = enPassant game
  p = side (b!i)
  cap j = onBoard j && b!j /= Nothing && side (b!j) /= p
  blank j = onBoard j && b!j == Nothing
  blankCap j = onBoard j && (b!j == Nothing || side (b!j) /= p)
  pawnStart = (p == White && y == 6) || (p == Black && y == 1)
  scan dx dy = unfoldr (\(x', y', cont) -> if cont && blankCap (x + x', y + y') then Just ((x + x', y + y'), (x' + dx, y' + dy, blank (x + x', y + y'))) else Nothing) (dx, dy, True)

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

#ifdef __HASTE__
box :: Int -> Int -> Int -> Int -> Picture ()
box x y dx dy = fill $ rect (fromIntegral x, fromIntegral y) (fromIntegral (x+dx), fromIntegral (y+dy))

sqColor False = RGB 191 191 191
sqColor True  = RGB 255 255 255

drawB pic x y = draw pic (fromIntegral x, fromIntegral y)

sym White King = "\x2654"
sym White Queen = "\x2655"
sym White Rook = "\x2656"
sym White Bishop = "\x2657"
sym White Knight = "\x2658"
sym White Pawn = "\x2659"
sym Black King = "\x265a"
sym Black Queen = "\x265b"
sym Black Rook = "\x265c"
sym Black Bishop = "\x265d"
sym Black Knight = "\x265e"
sym Black Pawn = "\x265f"

main = withElems ["canvas", "message", "promo"] $ \[canvasE, msg, promoSel] -> do
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

  ref <- newIORef undefined

  let
    shuffleIO [] = return []
    shuffleIO xs = getStdRandom (randomR (0, length xs - 1)) >>= \n ->
      let (a, b:bs) = splitAt n xs in (b:) <$> shuffleIO (a ++ bs)

    renderPiece c sq (x,y) = renderOnTop c $ font "40px sans-serif" $ text (fromIntegral x + 2, fromIntegral y + 35) (sym (side sq) (piece sq))

    drawGame game = let b = board game in do
      sequence_ $ (render buf $ draw boardCan (0, 0)) : [renderPiece buf sq (x*sz, y*sz) | i@(x, y) <- range bnds, let sq = b!i, sq /= Nothing]
      render canvas $ draw buf (0, 0)
      setProp msg "innerHTML" $ show (player game) ++ case state game of
        Play -> " to move"
        Won -> " wins"
        Draw -> " draws"

  let
    loop g = drawGame g >> writeIORef ref g
    newGame = loop initGame

  newGame

  let
    animate game = let b = board game in case anim game of
      Just (frame, m@(from@(x0, y0), (x1, y1))) ->
        if frame == 8 then let game1 = move game m in do
          drawGame game1
          -- Delay so canvas has a chance to update.
          if state game1 == Play && player game1 == Black then
            void $ setTimer (Once 20) $ do
              ms <- shuffleIO $ legalMoves game1
              animate game1 { anim = Just (0, best game1 ms) }
          else
            loop game1 { anim = Nothing }

        else do
          let f x0 x1 frame = x0 * sz + (x1 - x0) * sz * frame `div` 8
          drawGame game { board = b // [(from, Nothing)] }
          renderPiece canvas (b!from) (f x0 x1 frame, f y0 y1 frame)
          void $ setTimer (Once 20) $ animate game { anim = Just (frame + 1, m) }

  canvasE  `onEvent` MouseDown $ \(MouseData (bx, by) _ _) -> do
    game <- readIORef ref
    when (state game == Play && player game == White && anim game == Nothing) $ do
      let
        b = board game
        i@(x, y) = (div bx sz, div by sz)
        sel = if b!i /= Nothing && side (b!i) == player game then Just i else Nothing
      when (inRange bnds i) $ do
        render canvas $ draw buf (0, 0)
        case selection game of
          Nothing -> do
            unless (sel == Nothing) $ do
              renderOnTop canvas $ drawB fromCan (x*sz) (y*sz)
              sequence_ [renderOnTop canvas $
                drawB toCan (x1*sz) (y1*sz) | (x1, y1) <- legalMovesFrom i game]
            writeIORef ref game { selection = sel }
          Just sel0 | i `elem` legalMovesFrom sel0 game -> do
            s <- getProp promoSel "value"
            animate game { selection = Nothing, anim = Just (0, (sel0, i)), promoChoice = toPiece s }
          _ -> loop game { selection = Nothing }
  documentBody `onEvent` KeyDown $ \k -> when (k == 113) newGame
#endif
\end{code}
