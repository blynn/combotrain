= 15 Shades of Grey =

Arrange the grey squares so they appear from brightest to darkest going left to
right, top to bottom.

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<div id="playarea" tabindex="0" style="outline:none;">
<canvas id="canvas" style="border:1px solid black;display:block; margin:auto;"
  width="256" height="256"></canvas>
<p>
<div style="display:flex;justify-content:center;">
<button id="reshuffle">Reshuffle</button>
</div>
<div style="display:flex;justify-content:center;">
</p>
<p id="msg"></p>
</div>
</div>
<script>
function fillrect(ctx,x0,y0,x1,y1,colour) {
ctx.fillStyle=colour;
ctx.fillRect(x0,y0,x1,y1);
}
function setKeydown(f) { document.getElementById("playarea").onkeydown=
event => { event.stopPropagation(); event.preventDefault(); f(event); }; }
</script>

<script src="15.js"></script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

We write code that works with the https://www.haskell.org/ghc/[vanilla GHC
compiler], the https://haste-lang.org/[Haste compiler], and
https://github.com/tweag/asterius[the Asterius compiler].

Haste provides a library for common front-end tasks such as interacting with the HTML DOM, drawing
on a canvas, and handling events:

\begin{code}
{-# LANGUAGE CPP #-}
#ifdef __HASTE__
import Data.IORef
import Data.List
import Control.Monad
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
#endif
\end{code}

Asterius is more Spartan:

\begin{code}
#ifdef ASTERIUS
import Data.IORef
import Data.List
import Control.Monad
import Asterius.Types
#endif
\end{code}

The pure part of our code shuffles the board until we find a
https://en.wikipedia.org/wiki/15_puzzle#Solvability[solvable 15-puzzle
position].

We ought to use `uniformR` instead of `randomR` but we're supporting older
compilers.

For picking a random permutation uniformly, we could import
https://hackage.haskell.org/package/random-shuffle[System.Random.Shuffle], but
instead we implement our own to illustrate a point. The typical imperative
solution repeatedly swaps memory contents chosen from certain ranges and runs in
linear time. Although pure code is forbidden from such swaps, we can replace an
array of memory cells with a `Data.Map` that maps memory indexes to data
contents.  This sort of thing is based on trees, thus the running time
increases by a logarithmic factor.

Thus in general, we can construct a
https://en.wikipedia.org/wiki/Persistent_data_structure[persistent] version of
a data structure by paying at most a logarithmic factor.

Hopefully it is clear our shuffling routine has $O(N \log N)$ complexity. This
matches `System.Random.Shuffle`, but likely has a far higher constant factor
because a `Data.Map` is intended for more than just shuffling.

On a related note, a C programmer would represent a 2D board with an array. A
straightforward translation to Haskell is inefficient, because any array update
creates a fresh copy of the entire array. Better to use a map; to update a map
of $N$ entries only costs $O(\log N)$ more space.

\begin{code}
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import System.Random

type Board = Map (Int, Int) Int

shuf m g = M.fromList $ zip (M.keys m) $ shufElems m g
shufElems m g = case M.size m of
  0 -> []
  n -> let
    (i, g') = randomR (0, n - 1) g
    (_, v) = M.elemAt i m
    in v : shufElems (M.deleteAt i m) g'

side = 4
hole = side^2

goal = M.fromList $ zip ((,) <$> [0..side-1] <*> [0..side-1]) [1..]

parity []     = 0
parity (x:xs) = length (filter (x>) xs) + parity xs
              + if x == hole then uncurry (+) $ divMod (length xs) side else 0

gen :: IO Board
gen = do
  z <- shuf goal <$> newStdGen
  if even $ parity $ M.elems z then pure z else gen
\end{code}

Most of the code is taken by the front end, especially since we're animating
tiles. For Asterius, we define wrappers for JavaScript functions, including
some we wrote that can be studied by viewing the source of this page.

\begin{code}
#ifdef ASTERIUS
foreign import javascript "fillrect($1,$2,$3,$4,$5,$6)" fillrect :: JSVal -> Int -> Int -> Int -> Int -> JSString -> IO ()
foreign import javascript "setKeydown($1)" js_onKeyDown :: JSFunction -> IO ()
foreign import javascript "document.getElementById($1).onmousedown=$2" js_onMouseDown :: JSString -> JSFunction -> IO ()
foreign import javascript "document.getElementById($1).innerText=$2" js_setInner :: JSString -> JSString -> IO ()
foreign import javascript "$1.keyCode" keyCode :: JSVal -> Int
foreign import javascript "$1.offsetX" offsetX :: JSVal -> Int
foreign import javascript "$1.offsetY" offsetY :: JSVal -> Int
foreign import javascript "wrapper" mkCallback :: IO () -> IO JSFunction
foreign import javascript "wrapper" mkCallback1 :: (JSVal -> IO ()) -> IO JSFunction
foreign import javascript "wrapper oneshot" mkOnceCallback :: IO () -> IO JSFunction
foreign import javascript "setTimeout($1,$2)" js_setTimeout :: JSFunction -> Int -> IO ()
foreign import javascript "document.getElementById('canvas').getContext('2d')" mustCanvas :: IO JSVal

rgb r g b = toJSString $ concat ["rgb(", show r, ",", show g, ",", show b, ")"]
delayCall n f = flip js_setTimeout n =<< mkOnceCallback f
onKeyDown f = js_onKeyDown =<< mkCallback1 f
onMouseDown s f = js_onMouseDown (toJSString s) =<< mkCallback1 f
setInner elemId s = js_setInner (toJSString elemId) (toJSString s)
#endif
\end{code}

The Haste equivalents use the library functions bundled with the compiler:

\begin{code}
#ifdef __HASTE__
delayCall n f = void $ setTimer (Once n) f
rgb = RGB
onKeyDown f = void $ onEvent documentBody KeyDown f
onMouseDown s f = do
  Just e <- elemById s
  void $ onEvent e MouseDown f
setInner elemId s = do
  Just e <- elemById elemId
  setProp e "innerText" s
offsetX = fst . mouseCoords
offsetY = snd . mouseCoords
fillrect canvas x y w h c = let f = fromIntegral in
  renderOnTop canvas $ color c $ fill $ rect (f x, f y) (f $ x + w, f $ y + h)
mustCanvas = maybe undefined id <$> getCanvasById "canvas"
#endif
\end{code}

The rest of the code is the same for Haste and Asterius except for a
`preventDefault` call. The Haste edition prevents the default effect of key
down events if they are valid moves, so the browser behaves mostly as usual
during play and entirely as usual after winning.

With Asterius, calling `preventDefault()` from our code is difficult because
JavaScript and WebAssembly call one another, and events are handled in between;
by the time we call `preventDefault()` it is too late. We work around this with
a JavaScript helper eats key events in a certain focusable `div` element before
ceding control to wasm.

The user might have to click in a particular area to use the keys, and must
click outside in order to use typcial shortcuts such as Ctrl-R. To mimic our
Haste code, we could perhaps `preventDefault` for all key events on the
document body then somehow generate events that we want to pass on.

We mostly redraw only the two tiles affected by the animation. However, if a
move is interrupted by another move before its animation is complete, we redraw
the whole board before starting the new animation.

\begin{code}
#if defined(ASTERIUS) || defined (__HASTE__)
data Anim = Anim Int (Int, Int) (Int, Int)
data Game = Game (Maybe Anim) Board (Int, Int)

getColour n = rgb x x x where
  x | n == hole = 0
    | otherwise = 255 - (225 * (n - 1) `div` (side^2 - 1))

frameCount = 8
sz = 64

main = do
  canvas <- mustCanvas
  won <- newIORef undefined
  game <- newIORef undefined
  let
    win = do
      writeIORef won True
      setInner "msg" "A WINNER IS YOU"
    draw b = forM_ (M.assocs b) $ \((r, c), n) -> fillrect canvas (c*sz) (r*sz) sz sz $ getColour n
    loop = do
      Game a b cur <- readIORef game
      case a of
        Nothing -> when (b == goal) win
        Just (Anim frame tgt@(r1,c1) src@(r0,c0)) -> do
          let sc b a = a*sz + (b - a)*sz*frame`div`frameCount
          writeIORef game $ Game
            (if frame == frameCount then Nothing else Just $ Anim (frame + 1) tgt src) b cur
          fillrect canvas (c0*sz) (r0*sz) sz sz $ getColour hole
          fillrect canvas (c1*sz) (r1*sz) sz sz $ getColour hole
          fillrect canvas (sc c0 c1) (sc r0 r1) sz sz $ getColour $ b!src
          delayCall 16 loop
          
    newGame = do
      b <- gen
      let Just cur = fst <$> find ((== hole) . snd) (M.assocs b)
      writeIORef game $ Game Nothing b cur
      writeIORef won False
      setInner "msg" ""
      draw b
    move tgt (Game anim b cur) = do
      writeIORef game $ Game
        (Just $ Anim 0 tgt cur)
        (M.insert tgt hole $ M.insert cur (b!tgt) b)
        tgt
      case anim of
        Nothing -> loop
        Just _ -> draw b
    unlessWon f = readIORef won >>= flip unless f
  newGame

  onMouseDown "reshuffle" $ \_ -> newGame

  onMouseDown "canvas" $ \e -> unlessWon $ do
    let tgt@(r, c) = (div (offsetY e) sz, div (offsetX e) sz)
    g@(Game _ b (r0, c0)) <- readIORef game
    when (M.member (r, c) b && (c - c0)^2 + (r - r0)^2 == 1) $ move tgt g

  onKeyDown $ \k -> unlessWon $ do
    g@(Game _ b (r, c)) <- readIORef game
    let
      go tgt = when (M.member tgt b) $ do
#ifdef __HASTE__
        preventDefault
#endif
        move tgt g
    case keyCode k of
      38  -> go (r + 1, c)
      40  -> go (r - 1, c)
      37  -> go (r, c + 1)
      39  -> go (r, c - 1)
      _ -> pure ()
#endif
\end{code}

In this page, we use the output of the Haste compiler, which is significantly
smaller than the output of Asterius.

`hastec --opt-all 15.lhs`

------------------------------------------------------------------------
162153 15.js
------------------------------------------------------------------------

`ahc-link --bundle --browser --ghc-option -O --input-hs 15.lhs`

------------------------------------------------------------------------
 96075 15.js
811685 15.wasm
------------------------------------------------------------------------
