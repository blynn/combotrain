= 15 Shades of Grey =

Arrange the grey squares so they appear from brightest to darkest going left to
right, top to bottom.

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<canvas id="canvas" style="border:1px solid black;display:block; margin:auto;"
  width="256" height="256"></canvas>
<div style="display:flex;justify-content:center;">
<button id="reshuffle">Reshuffle</button>
</div>
<div id="msg" style="display:flex;justify-content:center;">
</div>
<script id="jsglue">"use strict";
const ctx = canvas.getContext("2d");
function rect(x0,y0,w,h,s) { ctx.fillStyle=s; ctx.fillRect(x0,y0,w,h); }
let run;
function initGame(repl) {
  run = function(s) { repl.run("chat", ["Main"], s); }
  reshuffle.addEventListener("click", ev => run("newGame"));
  canvas.addEventListener("click", ev => run("click " + ev.offsetX + " " + ev.offsetY));
  document.body.onkeydown = ev => {
    const k = ev.keyCode;
    if (37 <= k && k <= 40) {
      ev.preventDefault();
      run("keyDown " + k);
    }
  };
}
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

We target link:../compiler/[my Haskell compiler] which comes with a
browser-based interpreter and some hacks to aid web development.

== Every day I'm shuffling ==

We have a 4x4 board. We represent a position as a list of integers from 1 to
16, where 16 represents the hole into which tiles can be slid, and the tile at
row `r` and column `c` has index `4*r + c` in the list.

\begin{code}
side = 4
hole = side^2
goal = [1..hole]
oneD (r, c) = r*side + c
\end{code}

A new game requires us to permute the list randomly. Sampling a permutation
uniformly in linear time is easy with destructive updates, but less so in
Haskell (though perhaps linear types can help).
https://okmij.org/ftp/Haskell/perfect-shuffle.txt[Oleg Kiselyov describes [a
functional programming solution with O(N log N) complexity], which can be found
in the package
https://hackage.haskell.org/package/random-shuffle[System.Random.Shuffle] In
general, we can construct a
https://en.wikipedia.org/wiki/Persistent_data_structure[persistent] version of
a data structure by paying at most a logarithmic factor.

Since our list only has 16 entries, we're happy with a simple algorithm
with quadratic complexity.

\begin{code}
shuf xs = case length xs of
  0 -> pure []
  n -> do
    i <- fromInteger . readInteger <$> jsEval ("Math.floor(Math.random() * " ++ show n ++ ");")
    let (as, x:bs) = splitAt i xs
    (x:) <$> shuf (as ++ bs)
\end{code}

For example:

\begin{code}
print =<< shuf [0..9]
\end{code}

Not just any permutation will do.
https://en.wikipedia.org/wiki/15_puzzle#Solvability[A 15-puzzle position is
solvable if and only if it passes a certain parity check]:

\begin{code}
parity []     = 0
parity (x:xs) = length (filter (x>) xs) + parity xs
              + if x == hole then uncurry (+) $ divMod (length xs) side else 0
\end{code}

We generate a valid starting position with rejection sampling, that is, we keep
sampling until we find one that satisfies the parity condition.

\begin{code}
gen :: IO [Int]
gen = do
  z <- shuf goal
  if parity z `mod` 2 == 0 then pure z else gen
\end{code}

== Paint me a picture ==

Our compiler has non-standard `global` and `setGlobal` functions which
keep track of the game state, so that rather than write one giant `main`
function, we can write many little functions that are invoked on various
events. This suits web widgets. The following data structure holds the
game state, which we store globally.

\begin{code}
data Anim = Anim Int (Int, Int) (Int, Int)
data GameState = GameState
  { _board :: [Int]
  , _cursor :: (Int, Int)
  , _anim :: Maybe Anim
  }
\end{code}

The JavaScript on this page defines a few helpers for our code:

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<pre id="showglue"></pre>
<script>
showglue.innerText = jsglue.innerText;
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


A function to draw the current position:

\begin{code}
sz = 64
rgb r g b = concat ["rgb(", show r, ",", show g, ",", show b, ")"]
getColour n = rgb x x x where
  x | n == hole = 0
    | otherwise = 255 - (225 * (n - 1) `div` (side^2 - 1))
jsEval_ = (*> pure ()) . jsEval
square x y c = jsEval_ $ concat
  [ "rect(", show x, ", ", show y, ", ", show sz, ", ", show sz, ", ", show c, ");" ]
draw b = flip mapM_ (zip [0..] b) $ \(i, n) -> let
  (r, c) = divMod i side
  in square (c*sz) (r*sz) $ getColour n
\end{code}

The `loop` function handles animation: it draws a tile mid-slide, increments
the frame count, and schedules itself to be called again until the desired
number of frames have been drawn, upon which we check for victory.
We redraw only the two tiles affected by the animation.

\begin{code}
frameCount = 8
loop = do
  gst <- global
  case _anim gst of
    Nothing -> checkWin
    Just (Anim frame tgt@(r1,c1) src@(r0,c0)) -> do
      let sc b a = a*sz + (b - a)*sz*frame`div`frameCount
      setGlobal gst { _anim = if frame == frameCount then Nothing else Just $ Anim (frame + 1) tgt src }
      square (c0*sz) (r0*sz) $ getColour hole
      square (c1*sz) (r1*sz) $ getColour hole
      square (sc c0 c1) (sc r0 r1) $ getColour $ _board gst!!oneD src
      jsEval_ $ "setTimeout(() => run('loop'), 16);"

checkWin = do
  b <- _board <$> global
  when (b == goal) $ jsEval_ "msg.innerHTML = 'A WINNER IS YOU';"
\end{code}

To start a new game, we shuffle the board, draw it, and also check for victory
in the unlikely event that we randomly chose the identity permutation.

\begin{code}
newGame = do
  b <- gen
  let cur = fst $ head $ dropWhile ((/= hole) . snd) $ zip [0..] b
  setGlobal GameState
    { _board = b
    , _cursor = cur `divMod` side
    , _anim = Nothing
    }
  draw b
  jsEval_ "msg.innerHTML = '';"
  checkWin
\end{code}

Only the `move` function updates the position on the board. When it does so, it
kicks off the animation of the sliding tile. If there already is an animation
in progress, then we instantly complete the original animation so the already
scheduled `loop` will start the new animation. Otherwise we schedule a call to
`loop`.

\begin{code}
ins i x xs = as ++ x:bs where (as, _:bs) = splitAt i xs

move tgt = do
  gst <- global
  let i = oneD tgt
  let bo = ins (oneD $ _cursor gst) (_board gst!!i) $ ins i hole $ _board gst
  setGlobal gst
    { _board = bo
    , _cursor = tgt
    , _anim = Just $ Anim 0 tgt $ _cursor gst
    }
  case _anim gst of
    Nothing -> loop
    Just (Anim _ _ (r, c)) ->
        square (c*sz) (r*sz) $ getColour $ _board gst!!oneD (r, c)
\end{code}

These event handlers are called from JavaScript:

\begin{code}
isLegit (r, c) = 0 <= r && r < side && 0 <= c && c < side

click x y = do
  gst <- global
  when (_board gst /= goal) do
    let (r, c) = (div y sz, div x sz)
    let (r0, c0) = _cursor gst
    when (isLegit (r, c) && (c - c0)^2 + (r - r0)^2 == 1) $ move (r, c)

keyDown k = do
  gst <- global
  when (_board gst /= goal) do
    let
      (r, c) = _cursor gst
      go tgt = when (isLegit tgt) $ move tgt
    case k of
      38  -> go (r + 1, c)
      40  -> go (r - 1, c)
      37  -> go (r, c + 1)
      39  -> go (r, c - 1)
      _ -> pure ()
\end{code}

It remains to start a new game and connect our event handlers:

\begin{code}
newGame
jsEval_ "initGame(repl);"
\end{code}

Using `setTimeout` for animations is easy, but can cause problems. it is better
to use `requestAnimationFrame`, which we demonstrate elsewhere.
