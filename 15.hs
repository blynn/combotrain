import System.Random.Shuffle
import Control.Monad
import Data.Ix
import Data.Array
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Primitives

bnds = ((0,0), (3,3)); frameCnt = 8

data Anim = Done | Slide { frame :: Int, r0 :: Int, c0 :: Int, r :: Int, c :: Int } deriving Eq

key board sym = let
  (r0, c0) = head [i | i <- range bnds, board!i == 16]
  (r, c) = case sym of
      SDLK_UP    -> (r0 + 1, c0)
      SDLK_DOWN  -> (r0 - 1, c0)
      SDLK_LEFT  -> (r0, c0 + 1)
      SDLK_RIGHT -> (r0, c0 - 1)
      _          -> (-1, -1)
  in if inRange bnds (r, c) then Slide 0 r0 c0 r c else Done

parity [] = 0
parity (16:xs) = parity xs + (length xs `div` 4)
parity (x:xs) = sum [1 | y <- xs, x > y] + parity xs

gen = do x <- shuffleM [1..16]; if parity x `mod` 2 == 0 then return x else gen

isSolved board = and [board!i == 4*r + c + 1 | i@(r, c) <- range bnds]

main = withInit [InitEverything] $ do
  screen <- setVideoMode 128 128 32 [SWSurface]
  shuf <- gen
  let
    tile x y n = box screen (Rect x y (31 + x) (31 + y)) $ Pixel $ fromIntegral
      (255 + if n == 16 then 0 else ((255 - n * 15) * (2^24 + 2^16 + 2^8)))
    animate board Done = return (board, Done)
    animate board slide@(Slide frame r0 c0 r c) = do
      tile (32*c) (32*r) 16
      tile (32*c + 32*(c0 - c) * frame `div` frameCnt)
           (32*r + 32*(r0 - r) * frame `div` frameCnt) (board!(r,c))
      return (if frame == frameCnt - 1 then
        (board // [((r,c), board!(r0,c0)), ((r0,c0), board!(r,c))], Done) else
        (board, slide{frame = (frame + 1)}))
    loop board anim = do
      sequence_ [tile (32*c) (32*r) (board!(r, c)) | (r, c) <- range bnds]
      (board1, anim1) <- animate board anim
      SDL.flip screen
      (quit, anim2) <- eventLoop board1 anim1
      delay 10
      unless quit $ loop board1 anim2
    eventLoop board anim = do
      if isSolved board then putStrLn "A winner is you!" >> return (True, anim)
      else do
        event <- pollEvent
        case event of
          NoEvent -> return (False, anim)
          Quit    -> return (True, anim)
          KeyDown (Keysym SDLK_ESCAPE _ _) -> return (True, anim)
          KeyDown (Keysym sym _ _) -> eventLoop board
            $ if anim == Done then key board sym else anim
          _       -> eventLoop board anim
    in loop (array bnds [(i, shuf!!(4*r + c)) | i@(r, c) <- range bnds]) Done
