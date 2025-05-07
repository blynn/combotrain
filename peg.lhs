= Peg Solitaire =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<canvas id="canvas" width="280" height="280" style="display:block;margin:auto;">
</canvas>
<script id="jsglue">"use strict";
const ctx = canvas.getContext("2d");
function spot(x, y, r, sty) {
  ctx.fillStyle = sty;
  ctx.beginPath(); ctx.arc(x, y, r, 0, 2*Math.PI); ctx.fill();
}
function smiley(x, y, r) {
  ctx.fillStyle = 'rgb(255,255,255)';
  ctx.beginPath(); ctx.arc(x-r/4, y-r/4, 1.5, 0, 2*Math.PI); ctx.fill();
  ctx.beginPath(); ctx.arc(x+r/4, y-r/4, 1.5, 0, 2*Math.PI); ctx.fill();

  ctx.lineWidth = 2;
  ctx.strokeStyle = 'rgb(255,255,255)';
  ctx.beginPath(); ctx.arc(x, y, r/2, 1/6*Math.PI, 5/6*Math.PI); ctx.stroke();
}
function initGame(repl) {
  function run(s) { repl.run("chat", ["Main"], s); }
  canvas.addEventListener("mousedown", (ev) => { run("click " + ev.offsetX + " " + ev.offsetY); });
}
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

We represent the board with a `Map` from a pair of ints to a boolean.
The only other state we need is the current selected peg, if there is one.

\begin{code}
jsEval "curl_module('../compiler/Map.ob')"
\end{code}

\begin{code}
import Map
initBoard :: Map (Int, Int) Bool
initBoard = let f x = (x - 3)^2 <= 1 in fromList
  [((r, c), r /= 3 || c /= 3) | r <- [0..6], c <- [0..6], f r || f c]

act :: (Map (Int, Int) Bool, Maybe (Int, Int)) -> (Int, Int)
    -> (Map (Int, Int) Bool, Maybe (Int, Int))
act (st, sel) p@(r, c)
  | not $ member p st = (st, Nothing)
  | Nothing <- sel    = (st, if st ! p then Just p else Nothing)
  | p' == p           = (st, Nothing)
  | st ! p            = (st, Just p)
  | (r' - r)^2 + (c - c')^2 == 4, st ! m = (insert p' False $
    insert p True $ insert m False st, Just p)
  | otherwise         = (st, Nothing)
  where Just p'@(r', c') = sel
        m = (div (r + r') 2, div (c + c') 2)
\end{code}

This page contains JavaScript helpers to draw on the canvas and add event
listeners once our Haskell is compiled:

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<pre id="showglue"></pre>
<script>showglue.innerText = jsglue.innerText</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

We write code to draw the board, with a bigger circle around the selected peg
if it exists. If there is only one peg remaining and it lies in the center,
then we draw a smiley face on it.

\begin{code}
sz = 40 :: Int
rad = 12 :: Double
mid = div sz 2

jsEval_ = (*> pure ()) . jsEval

drawPeg ((x, y), b) = do
  jsEval_ $ concat
    [ "spot(", show (mid+x*sz), ", ", show (mid+y*sz), ", ", show rad
    , ", 'rgb(", show $ bool 0 255 b, ",0,0)');"
    ]
  
draw (board, sel) = do
  jsEval_ "ctx.clearRect(0, 0, canvas.width, canvas.height);"
  case sel of
    Just (x, y) -> jsEval_ $ concat
      [ "spot(", show (mid+x*sz), ", ", show (mid+y*sz), ", ", show $ rad + 4
      , ", 'rgb(0,127,0)');"
      ]
    Nothing -> pure ()
  mapM drawPeg $ assocs board
  when (map fst (filter snd $ assocs board) == [(3, 3)]) do
    jsEval_ $ concat
      [ "smiley(", show $ mid + 3*sz, ", ",  show $ mid + 3*sz
      , ", ", show rad, ");"
      ]
\end{code}

We make the board and selection available to all `IO` functions:

\begin{code}
refresh = global >>= draw

newGame = setGlobal (initBoard, Nothing) *> refresh

click x y = do
  g <- global
  setGlobal $ act g (div x sz, div y sz)
  refresh
\end{code}

Let's roll!

\begin{code}
newGame
jsEval_ "initGame(repl);"
\end{code}
