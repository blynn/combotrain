= Nim =

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<center>
<div id="winlose">Wins: 0 Losses: 0</div>
<canvas id="canvas" width="640" height="304"></canvas>
<canvas style="display:none;" id="snap" width="640" height="304"></canvas>
<div id="msg"></div>
<button id="newButton">New Game</button>
</center>
<script>"use script";
let ctx;
let wins = 0, losses = 0;

function victory() {
  wins++;
  msg.innerText = `You win!`;
  winlose.innerText = "Wins: " + wins + " Losses: " + losses;
}

function defeat() {
  losses++;
  msg.innerText = `Computer wins`;
  winlose.innerText = "Wins: " + wins + " Losses: " + losses;
}

function prep() {
  ctx = snap.getContext("2d");
  ctx.lineWidth = 1;
  ctx.clearRect(0, 0, snap.width, snap.height);
  ctx.fillStyle = 'rgb(255,215,0)';
  ctx.strokeStyle = 'rgb(0,0,0)';
}

function coin(x, y) {
  ctx.beginPath();
  ctx.rect(90*x - 80, 310 - 30*y, 70, 20);
  ctx.stroke(); ctx.fill();
}

function redraw() {
  ctx = canvas.getContext("2d");
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.drawImage(snap, 0, 0, canvas.width, canvas.height);
}

function highlight(x, y, n) {
  redraw();
  ctx.lineWidth = 4;

  for (let k = y; k <= n; k++) {
    ctx.beginPath();
    ctx.rect(90*x - 80, 310 - 30*k, 70, 20);
    ctx.stroke();
  }
}

let run;

function fade(x, y, n) {
  let alpha = 0;
  function go(t) {
    ctx.fillStyle = 'rgba(255,255,255,' + (alpha/32) + ')';
    ctx.beginPath();
    ctx.rect(90*x - 84, 0, 78, 334 - 30*y);
    ctx.fill();
    alpha++;
    if (alpha < 32) requestAnimationFrame(go); else run("postfade " + n);
  }
  requestAnimationFrame(go);
}

function setup(repl) {
  run = function(s) { repl.run("chat", ["Main"], s); };

  canvas.addEventListener("mousemove", ev => {
    const x = Math.floor((ev.offsetX + 80) / 90) - 1;
    const y = Math.floor((310 - ev.offsetY) / 30);
    run("mouseMove " + x + " " + y);
  });
  canvas.addEventListener("mouseout", ev => run("mouseOut"));
  canvas.addEventListener("click", ev => { run("play"); });
  function newGame() { run("newGame"); }
  newButton.addEventListener("click", ev => newGame());
  document.body.addEventListener("keydown", ev => { if (ev.keyCode == 113) newGame(); });
}
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The game of Nim starts with piles of coins. A popular starting position is
three piles containing 3, 5, and 7 coins, though any number of piles with
any number of coins will do.

On their turn, a player selects a pile and removes at least one of its coins;
they can take as many as they like, even the whole pile. A player who cannot
move loses the game, which happens when there are no coins left.

We could represent the piles of coins with a list of positive integers:

\begin{code}
nextNims :: [Int] -> [[Int]]
nextNims = \case
  [] -> []
  n:ns -> ns : map (:ns) [1..n-1] ++ map (n:) (nextNims ns)

nextNims [3,5,7]
\end{code}

but the order of the piles is irrelevant, so we instead use a map where a key
is a size of a pile, and its corresponding value is the number of piles with
that size. When updating a map, if a value drops to zero then its entry is
deleted.

\begin{code}
jsEval "curl_module('../compiler/Map.ob')"
\end{code}

\begin{code}
import Map

nextNims :: Map Int Int -> [Map Int Int]
nextNims m = [(if k == 1 then id else insert n (k - 1)) .
    (if i == 0 then id else insertWith (+) i 1) $ m' |
    (n, k) <- assocs m, let m' = delete n m, i <- [0..n-1]]

nextNims $ fromList $ map (,1) [1,2,3]
\end{code}

A standard minimax search finds the best move for any given position. However,
we can do much more since Nim is an _impartial_ game: the available moves (and
winning condition) depend only on the position, and not on whose turn it is.

We replace minimax with plain minimums, or perhaps we should say "minimin". The
symmetry of impartial games implies that a promising position for us on our
turn is equally promising for the opponent on their turn. We could negate the
score of a position every second ply and alternate between computing minimums
and maximums, but why not exploit the double negative? Simply use the same
scoring function for both sides, and always seek the minimum.

Each node of our game tree has a score of zero or one. Reaching zero means
we've reached a winning position: the opponent eventually will have no moves
available if we play perfectly. Reaching one means we've reached a losing
position: if the opponent plays perfectly then we ultimately lose.

Alpha-beta pruning degenerates to short-circuiting the computation of the
minimum on encountering a zero, and returning the other value on encountering
a one.

\begin{code}
data Tree a = Node a [Tree a]
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)
unfoldForest f = map (unfoldTree f)

minZ = foldr1 (\a b -> if a == 0 then 0 else b)

minOnly (Node _ kids) = case kids of
  [] -> 0
  _  -> 1 - minZ (minOnly <$> kids)

toPiles = concatMap $ uncurry $ flip replicate

bestBrute g = toPiles $ toAscList $ snd $ foldr1 go
    $ zip (minOnly . gameTree <$> xs) xs
  where
  xs = nextNims g
  go a b = if fst a == 0 then a else b
  gameTree = unfoldTree \g -> (g, nextNims g)
\end{code}

For example, if there is one pile of 5 coins, the best move is to take them
all:

\begin{code}
bestBrute $ fromList [(5, 1)]
\end{code}

For three piles with 2, 3, and 4 coins, it's best to take 3 from the biggest pile:

\begin{code}
bestBrute $ fromList $ (,1) <$> [2, 3, 4]
\end{code}

Adding a pile of \(n\) coins to a game only increases the number of possible
states by a factor of at most \(n\), so with memoization, it is feasible to
explore the entire game tree for typical games of Nim. We replace the tree data
structure with a lookup table, improving the running time exponentially:

\begin{code}
nimMinM m
  | size m == 0 = pure 0
  | otherwise = maybe go pure . mlookup as =<< get
  where
  as = toAscList m
  go = do
    n <- (1 -) . minZ <$> mapM nimMinM (nextNims m)
    modify $ insert as n
    pure n

nimScores = (`execState` mempty) . nimMinM . fromList
\end{code}

We print all zero-score positions of a `[3,5,7]` Nim game to produce a
cheat sheet. A winning strategy is to always move to a position on this list.

\begin{code}
do
  let
    tab = toAscList $ nimScores [(3,1), (5,1), (7,1)]
  print $ toPiles . fst <$> filter ((0 ==) . snd) tab
\end{code}

== Nimbers ==

"Moreover, there are many ways to be in error...But there is only one way to be
correct"
-- Aristotle, Nichomachean Ethics, Book 2

Each node of our game tree has a score of zero or one. If our move goes to a
zero node, then we can force a win: either there are no moves left, or the
opponent eventually will have no moves available if we play perfectly. Dually,
if our move goes to a one node, then we've reached a losing position: if the
opponent plays perfectly then we ultimately lose.

However, it turns out we profit from distinguishing between losing positions.
Rather than assign each a score of one, we associate a losing position with a
positive number called a _nimber_, or _Grundy value:_

  * All leaf positions have nimber 0. These are the positions where no moves are possible, so we win if we move to them.

  * All other nimbers are computed recursively. A position has nimber \(n\) if the set of nimbers that arise from all possible moves contains each of \(\{0, 1, ..., n-1\}\) and does not contain \(n\).

For example:

  * A position has nimber 1 if all moves directly lead to nimber 0 positions.

  * If \(n = 0\), the set is vacuously empty, so the condition simply becomes "does not contain zero", that is, if no moves go to a nimber 0 position, then the position has nimber 0.

  * A position has nimber \(4\) if a player can move to positions with nimbers in the set \(\{0, 1, 2, 3\}\).

  * A position has nimber \(4\) if a player can move to positions with nimbers in the set \(\{0, 1, 2, 3, 42\}\). Or \(\{0, 1, 2, 3, 5, 6, 7, 8\}\). Or any other set containing all naturals less than 4, but not containing 4 itself.

We find that like our first game tree, winning positions are associated with 0:
we can force a win if our move goes to a nimber 0 position. But unlike our
first game tree, the losing positions can have different positive numbers.

The smallest natural missing from a set of naturals is known as its _mex_
("minimum excluded value").


By replacing `(1 -) . minZ` with `mex` in our memoized game tree searcher,
we can find the nimber of every position:

\begin{code}
mex xs = head $ [0..] \\ xs

nimberNimM m
  | size m == 0 = pure 0
  | otherwise = maybe go pure . mlookup as =<< get
  where
  as = toAscList m
  go = do
    n <- mex <$> mapM nimberNimM (nextNims m)
    modify $ insert as n
    pure n

nimberMap = (`execState` mempty) . nimberNimM . fromList
\end{code}

Here's every nimber of every position of `[1,2,3]` Nim:

\begin{code}
first toPiles <$> toAscList (nimberMap [(1,1),(2,1),(3,1)])
\end{code}

In sum, nimbers are defined so that we can move to a position with a strictly
smaller nimber of our choice. But why bother distinguishing between losing
positions? Why would we ever want to move to a losing position with a
particular nimber?

== Two Wrongs Make a Right ==

Consider one-pile Nim. The winning strategy is obvious: take all the coins,
leaving nothing for the opponent, so they immediately lose. Almost by
definition, the nimber of one pile of \(n\) coins is \(n\).

Does one-pile Nim teach us anything about two-pile Nim? Let's study the nimbers
of two piles of various sizes:

\begin{code}
twoTab n = unlines
    [ unwords [sh $ twoNimber r c | c <- [0..n]] | r <- [0..n]]
  where
  -- twoPiles r c = toAscList $ fromListWith (+) $ filter ((/= 0) . fst) [(r,1), (c,1)])
  sh n
    | n < 10 = ' ':show n
    | otherwise = show n
  twoPiles r c
    | r > c = twoPiles c r
    | c == 0 = []
    | r == 0 = [(c, 1)]
    | r == c = [(r, 2)]
    | otherwise = [(r, 1), (c, 1)]
  twoNimber r c = maybe 0 id $ mlookup (twoPiles r c) cache
  cache = nimberMap [(n, 2)]

putStr $ twoTab 16
\end{code}

We study the table to get a feel for nimbers.
Let \(f(r, c)\) denote the entry at row \(r\) and column \(c\), where we start
counting rows and columns from zero.

A move in this game removes coins from either the \(r\) pile or the \(c\) pile,
so \(f(r,c)\) is the mex of the \(r\) nimbers above it and the \(c\) nimbers to
its left. The mex of a set containing at most \(r + c\) naturals is at most \(r
+ c\), thus \(f(r,c) \le r + c\). The definition of mex also implies a number
can never appear more than once in the same row, and similarly for columns.

We notice the first 4 rows and columns consist exclusively of the first 4
naturals, and this seems to hold for other powers of 2.

Let \(f(r, c)\) denote the entry at row \(r\) and column \(c\). Then we shall
prove that the first \(2^k\) rows and columns consist of the first \(2^k\)
naturals and moreover, for all \(2^k\) greater than \(r\) or \(c\):

\[
\begin{gathered}
f(      r, 2^k + c) = 2^k + f(r, c) \\
f(2^k + r,       c) = 2^k + f(r, c) \\
f(2^k + r, 2^k + c) = f(r, c)
\end{gathered}
\]

I couldn't find a short proof, and had to stitch together several fiddly
inductions. Assume the result for naturals less than \(k\). We have the
one-pile base case:

\[ f(2^k + r, 0) = 2^k + r \]

Then \(f(2^k + r, c)\) is the mex of
\[ \{ f(0, c), ..., f(2^k + r -1,c) \}
\cup \{ f(2^k + r, 0), ..., f(2^k + r, c-1) \} \]

By inductive hypothesis on \(c\), the right-hand set is:

\[ \{ 2^k + f(r, 0), ..., 2^k + f(r, c-1) \} \]

By inductive hypothesis on \(k\) and another on \(r\), the left-hand set is:

\[
\begin{gathered}
\{ f(0, c), ..., f(2^k-1, c) \} \cup
\{f(2^k,c), ..., f(2^k+r-1,c) \} \\
= [0..2^k-1] \cup \{ 2^k + f(0,c), ..., 2^k+f(r-1,c) \}
\end{gathered}
\]

Hence altogether, the mex is:

\[
\newcommand{\mex}{\mathop{\rm mex}\nolimits}
2^k + \mex \{ f(r,0), ..., f(r,c-1), f(0,c), ..., f(r-1,c) \}
 = 2^k + f(r,c) \]

A similar induction shows \(f (r,2^k + c) = 2^k + f(r,c) \) which puts us in a
position to prove, again by induction, that:

\[ f(2^k + r, 2^k + c) = f(r, c) \]

To cut a long story short, the function \(f\) is XOR, which we denote with
\((\oplus)\).

Now we understand why nimbers are helpful. For two-pile Nim, instead of
exploring a tree to figure out nimbers, we can instantly compute the nimber of
any given position by calculating the XOR of the nimbers of each pile, which
we've seen is simply the number of coins they contain.

Since the XOR of two numbers is zero exactly when they are equal, in two-pile
Nim, the winning strategy is to take coins from the bigger pile so it matches
the smaller pile. Of course, this is impossible when the piles are the same
size, in which case your opponent wins with correct play.

We see why we would want to move to a particular losing position, so to speak.
Suppose we have one pile of 5 coins and one pile of 3 coins. The winning move
is to take 2 coins from the pile of 5 coins, leaving 3 coins in each. In
one-pile Nim, taking 2 from 5 is a losing move because it leaves behind 3
coins, a losing position. But composing it with another losing position with
the same nimber results in a winning position.

== From minimax to minimum to mex to XOR ==

We've seen how a game of two-pile Nim can be composed from two games of
one-pile Nim. We generalize what we did to any two impartial games \(G\) and
\(H\).

Define \(G + H\) to be the game where each turn consists of either making a
move in the position \(G\) to get \(G'\), or making a move in the position
\(H\) to get \(H'\). A player must make a legal move in exactly one of the
two games, otherwise they lose; the resulting position must be one of
\(G' + H\) or \(G + H'\).
(We're being a bit loose with "game" versus "position".)

For example, if \(G\) and \(H\) are one-pile Nim games, then \(G + H\) is a
two-pile Nim game.

All our reasoning applies to any impartial games \(G\) and \(H\). That is, let
\(g, h\) be the nimbers of \(G, H\). Then the nimber of \(G + H\) is \(g \oplus
h\).

If \(g \oplus h \ne 0\), then they are unequal and we should make a move that
reduces the larger nimber to match the smaller. Our opponent is then forced to
make a move resulting in unequal nimbers. Iterating this process leads to a
win, if the game is guaranteed to terminate.

For example, in any game of Nim, by induction, the nimber of a position is the
XOR of all pile sizes.

\begin{code}
nimber = foldr xor (0 :: Int)

best (n:ns) 
  | n == h = Nothing
  | otherwise = go n ns h
  where
  h = nimber ns
  go g xs h
    | g > h = Just (g, h)
    | otherwise = case xs of
      x:xt -> go x xt $ g `xor` h `xor` x
\end{code}

No trees. No memoization. A handful of XORs is all we need to play Nim
perfectly.

Nonetheless, our tree search has not been in vain. Other impartial games are
difficult to decompose, and computing their nimbers requires walking through a
game tree.

== Game On ==

Finding the nimber of a Nim position takes one line of code. Finding the best
move takes a few more. But demonstrating them with a web widget takes an
eye-watering mess:

\begin{code}
data Nim = Nim
  { _nim :: [Int]
  , _selection :: Maybe (Int, Int)
  , _busy :: Bool
  }

draw ns = do
  jsEval_ "prep();"
  go 1 ns
  jsEval_ "redraw();"
  where
  go x = \case
    [] -> pure ()
    n:nt -> do
      sequence [jsEval $ concat
        [ "coin(", show x, ", ", show y, ");" ] | y <- [ 1..n]]
      go (succ x) nt

mouseOut = do
  g <- global
  setGlobal g { _selection = Nothing }
  unless (_busy g) $ jsEval_ "redraw();"

mouseMove x y = do
  g <- global
  let ns = _nim g
  if x >= 0 && y >= 0 && x < length ns && y < ns!!x
    then do
      setGlobal g { _selection = Just (x, y) }
      unless (_busy g) $ jsEval_ $ concat
        [ "highlight(", show $ x+1, ", ", show $ y+1, ", ", show $ ns!!x, ");" ]
    else do
      setGlobal g { _selection = Nothing }
      unless (_busy g) $ jsEval_ "redraw();"

play = do
  g <- global
  unless (_busy g) case _selection g of
    Nothing -> pure ()
    Just (x, y) -> do
      let ns = _nim g
      let (us, n:vs) = splitAt x ns
      setGlobal g { _nim = us ++ y:vs, _busy = True }
      jsEval_ "msg.innerText = `Computer's move`;"
      jsEval_ $ concat
        ["fade(", show (x+1), ", ", show (y+1), ", 0);"]

rand n = fromInteger . readInteger <$> jsEval ("Math.floor(Math.random() * " ++ show n ++ ");")

yourMove = do
  jsEval_ "msg.innerText = `Your move`;"
  _selection <$> global >>= \case
    Nothing -> pure ()
    Just (x, y) -> mouseMove x y

postfade 0 = do
  g <- global
  let ns = _nim g
  draw ns
  let
    move n n' = do
      draw ns
      let (us, _:vs) = break (n ==) ns
      setGlobal g { _nim = us ++ n':vs }
      jsEval_ $ concat
        ["fade(", show (length us + 1), ", ", show (n'+1), ", 1);"]

  case best ns of
    Nothing -> case filter (0 /=) ns of
      [] -> do
        setGlobal g { _busy = False }
        jsEval_ "victory();"
      ms -> do
        m <- (ms!!) <$> rand (length ms)
        m' <- rand m
        move m m'
    Just (n, n') -> move n n'

postfade 1 = do
  g <- global
  let ns = _nim g
  draw ns
  setGlobal g { _busy = False }
  if all (0 ==) ns then jsEval_ "defeat();" else yourMove

startGame ns = do
  setGlobal $ Nim ns Nothing False
  draw ns
  yourMove

newGame = do
  p <- succ <$> rand 7
  ns <- replicateM p $ succ <$> rand 10
  startGame ns

jsEval_ "setup(repl);"
startGame [3, 5, 7]
\end{code}

== The Sprague-Grundy Theorem ==

The above results on impartial games are known as
https://en.wikipedia.org/wiki/Sprague%E2%80%93Grundy_theorem[the
Sprague-Grundy theorem].

I presented the ideas in a strange order because I wanted to relate them to my
other notes on game trees, and also to tell an optimization story. We started
with standard minimax, but by changing the scoring function and reasoning a
little, we ended with an algorithm whose running time is within a small
constant factor of the time it takes to read the input.

A more natural path would be to first consider one-pile Nim, which is trivial,
and then consider two-pile Nim. Most people would likely stumble upon the
winning strategy of always leaving an equal number of coins in both piles.
Next, we indulge in some
https://en.wikipedia.org/wiki/Centipede_mathematics["centipede mathematics"]:
we remove some properties of pile sizes and see if the proof still works,
hoping to find something that works more generally.

Given a sum of games \(G + H\), suppose we have some way of associating a
natural number with each position of each game such that:

  * If \(g < h\), there exists a move that reduces \(h\) to \(g\).
  * If \(g > h\), there exists a move that reduces \(g\) to \(h\).
  * If \(g = h\), no move leaves \(g\) and \(h\) alone.

where \(g\) and \(h\) are the naturals associated with the positions \(G\) and
\(H\). (Again, we're being sloppy here, and reusing the same variable to
mean the natural associated with the next position of a game.)
  
Then if \(G + H\) is guaranteed to terminate, we win if we always move to a
position such that \(g = h\),

These conditions capture the essence of two-pile Nim strategy, but there is
another problem we must tackle, one faced by every mathematician and computer
scientist when decomposing a big problem into small ones. Every piece must be
self-contained: the number \(g\) is computed from \(G\) alone. The legality of
a move in one game is independent of the state of the other.

The straitjacket of composition pushes us to the following. Let \(g\) be a
natural associated with the position \(G\).

  * For all \(g' < g\), there exists a move that reduces \(g\) to \(g'\).
  * No move preserves \(g\).

Look familiar? These conditions define nimbers, and they imply a game only
finishes when \(g = h = 0\).

Notice throughout the above, we have no objection to a move that increases the
natural associated with a position. The proof for the winning strategy works
either way. We only care that no move preserves the nimber, and that there
exists a move to reach any desired smaller nimber.

We could have tried removing more properties. For example:

  * If \(g\) is nonzero, there exists a move that reduces \(g\) to 0.
  * No move preserves \(g\).
  * Positions with no moves are associated with 0.

This is equivalent to the game tree we started with, if we consider 1 to
represent nonzero. It works, but this time, we are unable to compute the
natural associated with \(G + H\) from \(g\) and \(h\) alone. If at least one
of \(g\) and \(h\) are zero then there's no problem, but even two-pile Nim has
winning and losing positions where both are nonzero.

Sprague and Grundy found the sweet spot. If we remove too few properties, then
there are too many restrictions for our ideas to apply more generally. If we
remove too many, our ideas may apply more generally, but we lack enough data to
solve big problems via composition.

See https://en.wikipedia.org/wiki/Winning_Ways_for_Your_Mathematical_Plays[Berlekamp, Conway, and Guy, _Winning Ways for Your Mathematical Plays_].
