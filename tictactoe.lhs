= Tic-Tac-Toe =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Algorithm:
<div>
<input type="radio" name="preset" id="alphabeta" checked="true">Minimax with alpha-beta pruning<br>
<input type="radio" name="preset" id="brute">Minimax<br>
<input type="radio" name="preset" id="rando">Random<br>
<input type="radio" name="preset" id="first">First Available<br>
</div>
<div style="text-align:center;">
<p>
<button id="newButton">New Game</button>
</p>
<canvas id="canvas" style="border:1px solid black;display:block;margin:auto;" width="192" height="192"></canvas>
<div id="message"></div>
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Tic-tac-toe, or Noughts and Crosses, is the "Hello, World!" of two-player
deterministic perfect information turn-based games. (This sentence remains true
after removing an arbitrary number of adjectives.) Thus this game is ideal for
experimenting with computer opponents that search game trees.

== Get on board ==

We represent a board with a list of 9 integers, where 0 means an empty square,
while 1 and -1 mean X and O. We also record the next player to move and whether
someone as won. Although this could be deduced from the board alone, since X
always moves first in our version, it seems better to compute this information
once and store the result.

When the game is won, we reuse the next-player field to store the winning
player.

\begin{code}
data Status = Draw | Won | Play deriving (Eq, Show)
data Game = Game [Int] Status Int deriving Show

initGame = Game (replicate 9 0) Play (-1)

goals =
  [ [0,1,2]
  , [3,4,5]
  , [6,7,8]
  , [0,3,6]
  , [1,4,7]
  , [2,5,8]
  , [0,4,8]
  , [2,4,6]
  ]

classify board player
  | or $ all ((== player) . (board!!)) <$> goals = Game board Won  player
  | 0 `notElem` board                            = Game board Draw player
  | otherwise                                    = Game board Play $ -player
\end{code}

From a given board, the next possible moves are determined by the empty spots,
which is slightly awkward to compute with lists:

\begin{code}
nextMoves game@(Game board status p) = case status of
  Play -> (`classify` p) <$> go id board
  _    -> []
  where
  go pre post = case post of
    []   -> []
    x:xs -> (if x == 0 then (pre (p:xs) :) else id) $ go (pre . (x:)) xs
\end{code}

== The Game Tree ==

Even non-gamers likely understand game trees, as in everyday interactions, we
think "if I do A, then they'll do X, but if I do B, then they'll do Y or Z".

We swipe some code from `Data.Tree`, and define `maximum` and `minimum` which
our `Base` library lacks:

\begin{code}
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: [Tree a]   -- ^ zero or more child trees
    } deriving Show

unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

unfoldForest :: (b -> (a, [b])) -> [b] -> [Tree a]
unfoldForest f = map (unfoldTree f)

maximum = foldr1 max
minimum = foldr1 min
\end{code}

Computing the game tree from a given board is one-liner:

\begin{code}
gameTree = unfoldTree \g -> (g, nextMoves g)
\end{code}

For example:

\begin{code}
gameTree (Game
  [-1,1,0
  ,-1,1,1
  ,0,-1,0] Play 1)
\end{code}

== Best of the worst ==

The idea behind minimax comes naturally to gamers. If both players are playing
optimally, and know that their opponent is playing optimally, then we should
assume our opponent is going to find the move that hurts the most; the
worst-case scenario. On our end, we always want the move with the most benefit.
Thus we end up alternating between two points of view, hence the name minimax.

Implementing minimax is comes less naturally because mutual recursion can be
confusing. Luckily, we're writing in Haskell, and luckier still, we're only
one click away from the classic paper:
https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf[John Hughes,
_Why Functional Programming Matters_].

He shows how easy it is to implmement minimax search in Haskell. The game tree
one-liner above is also featured in the paper.

\begin{code}
score (Game _ Won n) = n
score _              = 0

maximize (Node leaf [])   = score leaf
maximize (Node _    kids) = maximum $ minimize <$> kids

minimize (Node leaf [])   = score leaf
minimize (Node _    kids) = minimum $ maximize <$> kids
\end{code}

Given a list of potential moves `xs`, the following returns the first best move
found:

\begin{code}
best xs = snd $ foldr1 go $ zip (minimize . gameTree <$> xs) xs
  where
  go a b = if fst a > fst b then a else b
\end{code}

But wait, there's more! Hughes then demonstrates the power of lazy evaluation
by implementing alpha-beta pruning, an optimization which also comes naturally
to gamers. If I'm playing chess, and I see a move that leads to the opponent
checkmating me, then I'll immediately drop that move from further
consideration; it makes no sense to play it anyway and hope my opponent fails
to spot a win. This is a special case of alpha-beta pruning.

As Hughes explains, thanks to lazy evaluation, a few edits are enough to gain
alpha-beta pruning. In other languages, we must dive deep into the search
algorithm to change its control flow.

\begin{code}
omitWith op (ms:mss) = m : omit m mss where
  oppest = foldr1 \a b -> if op a b then a else b
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

bestAB ms = snd $ foldr1 go $ zip (minimum . minimize' . gameTree <$> ms) ms
  where
  go a b = if fst a > fst b then a else b
\end{code}

== Plumbing ==

Now to put the above theory into practice.

We shuffle the list of potential moves so that the best one chosen by the
search varies from game to game.

\begin{code}
shuffle xs = case length xs of
  0 -> pure []
  n -> do
    i <- fromInteger . readInteger <$> jsEval ("Math.floor(Math.random() * " ++ show n ++ ");")
    let (as, x:bs) = splitAt i xs
    (x:) <$> shuffle (as ++ bs)
\end{code}

We make various algorithms available for the user to try out.

\begin{code}
aiMove game = do
  shuffled <- shuffle future
  go
    [ ("alphabeta", bestAB shuffled)
    , ("brute", best shuffled)
    , ("rando", head shuffled)
    , ("first", head future)
    ]
  where
  future = nextMoves game
  go ((s, m):rest) = do
    jsEval (s ++ ".checked;") >>= \case
      "true" -> pure m
      _ -> go rest
\end{code}

Code to draw the board:

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script>"use strict";
const xo = new Image();
xo.src = `data:image/webp;base64,
include::xo.base64[]
`;
xo.addEventListener("load", (ev) => console.log(ev));
const ctx = canvas.getContext("2d");
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

\begin{code}
sz = 64
bd = 4
clip x y xoff = jsEval_ $ concat
  [ "ctx.drawImage(xo, ", show xoff, ", 0, ", show sz, ", ", show sz
  , ", ", show x, ", ", show y, ", ", show sz, ", ", show sz
  , ");"
  ]

sq i p = when (p /= 0) $ clip (x*sz) (y*sz) (bool 0 sz $ p > 0) where
  (y, x) = divMod i 3

draw = (*> pure ()) . sequence . zipWith sq [0..]
\end{code}

Event handlers and user interface. The `delayHack` mess allows the webpage to
be redrawn so that the player can see their move while the computer is
thinking.

\begin{code}
update game@(Game board status player) = do
  setGlobal game
  draw board
  jsEval_ $ concat ["message.innerHTML = `"
    , case status of
      Won  -> ("X.O"!!(player + 1)) : " wins"
      Draw -> "Draw"
      Play -> if player == -1 then "X to move" else "Thinking..."
    , "`;"
    ]
  when (player == 1 && status == Play) do
    jsEval_ $ "console.log(`" ++ show (nextMoves game) ++ "`);"
    delayHack $ update =<< aiMove game

valid x y = and [0 <= x, x <= 3, 0 <= y, y <= 3]

click xraw yraw = do
  Game board status player <- global
  let
    x = div xraw sz
    y = div yraw sz
  when (status == Play && player == -1 && valid x y) do
    -- I originally called it `x` but discovered my compiler has
    -- a bug involving shadowed variables.
    let (as, totallyNotX:bs) = splitAt (3*y + x) board
    when (totallyNotX == 0) $ update $ classify (as ++ player:bs) player

delayRef = unsafePerformIO $ newIORef (undefined "delayHack" :: IO ())
delayHack f = do
  writeIORef delayRef f
  jsEval_ $ "setTimeout(() => repl.run('chat', ['Main'], 'delayHackCont'), 1);"
delayHackCont = readIORef delayRef >>= id
\end{code}

The `newGame` function draws the borders of the board, and calls `update` with
an empty board.

\begin{code}
oblong x y w h = jsEval_ $ concat
  ["ctx.fillRect(", show x, ", ", show y, ", ", show w, ", ", show h, ");"]

newGame = do
  jsEval_ "ctx.clearRect(0, 0, canvas.width, canvas.height);"
  oblong (  sz - bd) 0 (2*bd) (3*sz)
  oblong (2*sz - bd) 0 (2*bd) (3*sz)
  oblong 0 (  sz - bd) (3*sz) (2*bd)
  oblong 0 (2*sz - bd) (3*sz) (2*bd)
  update initGame
\end{code}

We hook up the event handlers and start a new game:

\begin{code}
jsEval_ [r|
  newButton.addEventListener("click", (ev) => repl.run("chat", ["Main"], "newGame"));
  canvas.addEventListener("click", (ev) => repl.run("chat", ["Main"], "click " + ev.offsetX + " " + ev.offsetY));
  document.body.addEventListener("keydown", (ev) => { if (ev.keyCode == 113) repl.run("chat", ["Main"], "newGame"); });
|]
newGame
\end{code}
