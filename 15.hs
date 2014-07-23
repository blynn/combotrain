import Control.Monad
import Data.Ix
import Data.Array
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Primitives

bnds = ((0,0), (3,3))

key board sym = let
  (r0, c0) = head [i | i <- range bnds, board!i == 16]
  (r, c) = case sym of
      SDLK_UP    -> (r0 + 1, c0)
      SDLK_DOWN  -> (r0 - 1, c0)
      SDLK_LEFT  -> (r0, c0 + 1)
      SDLK_RIGHT -> (r0, c0 - 1)
      _          -> (-1, -1)
  in if inRange bnds (r, c) then
      board // [((r,c), board!(r0,c0)), ((r0,c0), board!(r,c))]
    else board

main = withInit [InitEverything] $ do
  screen <- setVideoMode 128 128 32 [SWSurface]
  let
    loop board = do
      sequence_ [let x = c*32; y = r*32; n = board!(r,c) in
        box screen (Rect x y (31 + x) (31 + y)) $ Pixel $ fromIntegral (255 +
        if n == 16 then 0 else ((255 - n * 15) * (2^24 + 2^16 + 2^8)))
        | (r, c) <- range bnds]
      SDL.flip screen
      delay 32
      (quit, board1) <- eventLoop board
      unless quit $ loop board1
    eventLoop board = do
      event <- pollEvent
      case event of
        Quit    -> return (True, board)
        NoEvent -> return (False, board)
        KeyDown (Keysym SDLK_ESCAPE _ _) -> return (True, board)
        KeyDown (Keysym sym _ _) -> eventLoop $ key board sym
        _       -> eventLoop board
    in loop $ array bnds [(i, 4*r + c + 1) | i@(r, c) <- range bnds]
