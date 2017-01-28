import System.Random.Shuffle
import Control.Monad
import Data.Array
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Primitives

bnds = ((0,0), (3,3)); frameCnt = 8; sz = 64

data Anim = Ready | Solved | Slide { frame :: Int, r0 :: Int, c0 :: Int, r :: Int, c :: Int } deriving Eq

key board sym = let
  (r0, c0) = head [i | i <- range bnds, board!i == 16]
  (r, c) = case sym of
      SDLK_UP    -> (r0 + 1, c0)
      SDLK_DOWN  -> (r0 - 1, c0)
      SDLK_LEFT  -> (r0, c0 + 1)
      SDLK_RIGHT -> (r0, c0 - 1)
      _          -> (-1, -1)
  in if inRange bnds (r, c) then Slide 0 r0 c0 r c else Ready

parity [] = 0
parity (16:xs) = parity xs + (length xs `div` 4)
parity (x:xs) = length (filter (x>) xs) + parity xs

gen = do x <- shuffleM [1..16]; if parity x `mod` 2 == 0 then return x else gen

isSolved board = and [board!i == 4*r + c + 1 | i@(r, c) <- range bnds]

main = withInit [InitEverything] $ do
  screen <- setVideoMode (4*sz) (4*sz) 32 [SWSurface]
  let
    tile x y n = box screen (Rect x y (sz-1 + x) (sz-1 + y)) $ Pixel $ fromIntegral
      (255 + if n == 16 then 0 else ((255 - 15*(n - 1)) * (2^24 + 2^16 + 2^8)))
    animate board Ready = if isSolved board then putStrLn "A winner is you!" >> return (board, Solved) else return (board, Ready)
    animate board Solved = return (board, Solved)
    animate board slide@(Slide frame r0 c0 r c) = do
      tile (sz*c) (sz*r) 16
      tile (sz*c + sz*(c0 - c) * frame `div` frameCnt)
           (sz*r + sz*(r0 - r) * frame `div` frameCnt) (board!(r,c))
      return (if frame == frameCnt - 1 then
        (board // [((r,c), board!(r0,c0)), ((r0,c0), board!(r,c))], Ready) else
        (board, slide{frame = (frame + 1)}))
    loop board anim = do
      sequence_ [tile (sz*c) (sz*r) (board!(r, c)) | (r, c) <- range bnds]
      (board1, anim1) <- animate board anim
      SDL.flip screen
      (quit, anim2) <- eventLoop board1 anim1
      delay 10
      unless quit $ loop board1 anim2
    eventLoop board anim = do
      event <- pollEvent
      case event of
        NoEvent -> return (False, anim)
        Quit    -> return (True, anim)
        KeyDown (Keysym SDLK_ESCAPE _ _) -> return (True, anim)
        KeyDown (Keysym sym _ _) -> eventLoop board $
          if anim /= Ready then anim else key board sym 
        _       -> eventLoop board anim
    newGame = do
      x <- gen
      loop (array bnds [(i, x!!(4*r + c)) | i@(r, c) <- range bnds]) Ready
    in newGame
