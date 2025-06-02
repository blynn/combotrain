= Summer Games =

+++<div style="display:none;" id="howtoXXX">+++
*Treblecross*: Tic-tac-toe. One long row. Both play X. There is no O.
+++</div>+++

+++<div style="display:none;" id="howtoChess">+++
*Dawson's Chess*: Captures are mandatory. A player loses if unable to move.
+++</div>+++

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<div id="howto"></div>
<br>
<style>
.center{display:block;margin:auto;text-align:center;}
</style>
<canvas style="display:none;" id="xxxCanvas" class="center" width="640" height="30"></canvas>
<canvas id="chessCanvas" class="center" style="display:none;border:1px solid black;" width="640" height="120"></canvas>
<br>
<div id="msg" class="center"></div>
<div class="center">New game: <button id="xxxButton">XXX</button> <button id="chessButton">&#x2659;&#x265f;</button></div>
<br>
<script>"use script";
const x0 = 5, y0 = 5, dx = 20, dy = 20;

function drawTreble(n) {
  const ctx = xxxCanvas.getContext("2d");
  ctx.clearRect(0, 0, xxxCanvas.width, xxxCanvas.height);
  for (let i = 0; i < n; i++) {
    ctx.beginPath();
    ctx.rect(x0 + i*dx, y0, dx, dy);
    ctx.stroke();
  }
}

function ecks(n) {
  const ctx = xxxCanvas.getContext("2d");
  ctx.beginPath();
  ctx.moveTo(x0 + n*dx, y0);
  ctx.lineTo(x0 + n*dx + dx, y0 + dy);
  ctx.moveTo(x0 + n*dx + dx, y0);
  ctx.lineTo(x0 + n*dx, y0 + dy);
  ctx.stroke();
}

let run;
function initXXX(repl) {
  run = s => repl.run("chat", ["Main"], s);
  xxxCanvas.addEventListener("click", ev => run("click " + ev.offsetX + " " + ev.offsetY));
  xxxButton.addEventListener("click", ev => run("newXXX"));
}

const cc = chessCanvas.getContext("2d");
function checker(c, x, y, w, h) {
  cc.fillStyle = c == 0 ? 'rgb(255,255,255)' : 'rgb(191,191,191)';
  cc.beginPath();
  cc.rect(x,y,w,h);
  cc.fill();
}
function pawn(c, x, y) {
  cc.font = "40px sans-serif";
  cc.fillStyle = 'rgb(0,0,0)';
  cc.fillText(c, x, y);
}
const fromCanvas = document.createElement('canvas');
const toCanvas = document.createElement('canvas');
function initChess(sz, d) {
  {
    fromCanvas.width = sz;
    fromCanvas.height = sz;
    const c = fromCanvas.getContext('2d');
    c.fillStyle = 'rgb(127,15,15)';
    c.beginPath();
    c.rect(0, 0, d, sz);
    c.rect(0, 0, sz, d);
    c.rect(sz - d, 0, d, sz);
    c.rect(0, sz - d, sz, d);
    c.fill();
  }
  {
    toCanvas.width = sz;
    toCanvas.height = sz;
    const c = toCanvas.getContext('2d');
    c.fillStyle = 'rgba(0,191,0,0.3)';
    c.beginPath();
    c.rect(0, 0, sz, sz);
    c.fill();
  }
  chessCanvas.addEventListener("click", ev => run("chessClick " + ev.offsetX + " " + ev.offsetY));

  chessButton.addEventListener("click", ev => run("newChess"));
}
const snapChessCanvas = document.createElement('canvas');
snapChessCanvas.width = chessCanvas.width;
snapChessCanvas.height = chessCanvas.height;
function snapChess() { snapChessCanvas.getContext("2d").drawImage(chessCanvas, 0, 0); }
function unsnapChess() { chessCanvas.getContext("2d").drawImage(snapChessCanvas, 0, 0); }
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Let's recap link:nim.html[our notes on the game of Nim].

Every position \(X\) of any impartial game can be mapped to a natural number
\(\newcommand{\nimber}[1]{\mathcal{G}(#1)}\nimber{X}\) known as its nimber, or
Grundy value. Nimbers can be defined recursively on the game tree:
the nimber of a node is the mex of the nimbers of its children.

The following `mexM` function computes nimbers with memoization to avoid
duplicating work:

\begin{code}
jsEval "curl_module('../compiler/Map.ob')"
\end{code}

\begin{code}
mex :: [Int] -> Int
mex xs = head $ [0..] \\ xs

import Map

mexM f n = maybe go pure . mlookup n =<< get where
  go = do
    m <- mex <$> mapM (mexM f) (f n)
    modify $ insert n m
    pure m
\end{code}

In general, this is little better than minimax search, which also explores a
tree to discover all winning positions. However, nimbers allow shortcuts if we
can decompose a game into a sum of games, for the nimber of the sum of
positions is the XOR of the nimbers of each summand. For example, a Nim
position is especially simple: its nimber is the XOR of the sizes of each pile.

Another application of nimbers is to play optimally in a Frankenstein game of,
say, Nim plus Kayles plus Treblecross, that is, we start the three impartial
games simultaneously and a turn consists of making a move in one of the three
positions; a player loses if unable to move in any of the three.

We follow
https://en.wikipedia.org/wiki/Winning_Ways_for_Your_Mathematical_Plays[Berlekamp,
Conway, and Guy, _Winning Ways for Your Mathematical Plays_], and analyze
Nim-like games that naturally decompose into sums of games. While we cannot
compute their nimbers as efficiently as we can for Nim, we can compute enough
to trounce an uninitiated player for realistic game sizes.

== Subtraction Games ==

Consider a variant of Nim where a player can take 1, 2, or 3 coins.

When there is one pile, we win if we always leave a multiple of 4 coins,
which we can confirm with brute force. For example:

\begin{code}
nextSubs ds n = takeWhile (>= 0) $ (n -) <$> ds

toAscList $ (`execState` mempty) $ mexM (nextSubs [1,2,3]) 16
\end{code}

When there are multiple piles, as with regular Nim, the nimber of the position
is the XOR of the nimbers of each pile.

We've written the code so it's easy to try other _subtraction sets_.
For example, suppose a player must remove 2, 5, or 6 coins from a pile:

\begin{code}
toAscList $ (`execState` mempty) $ mexM (nextSubs [2,5,6]) 16
\end{code}

The nimber for 15 coins is missing because a player must take at least 2 coins.

We'd like to examine all positions up to a given size, so we write a bottom-up
version of the above. We hold the nimbers found so far in a list, and treat
the smallest piles as special cases so we can usually avoid size checks.

The `go` helper adds the next nimber to the front of the list, causing the
nimbers to appear in reverse order. We assume the `ds` list is given in
increasing order.

\begin{code}
subNimbers' ds = iterate go $ fst $ iterate tiny ([], 0) !! last ds where
  go acc = mex ((acc!!) . pred <$> ds):acc
  tiny (acc, len) = (mex [acc!!d | d <- pred <$> ds, d < len]:acc, succ len)

subNimbers ds n = reverse $ subNimbers' ds !! (max 0 $ succ n - last ds)

subNimbers [1,2,3] 32
subNimbers [2,5,6] 32
\end{code}

It looks like the nimbers are periodic, and indeed, they always are for any
subtraction game.

If \(m\) is the maximum of number of coins we can take, then \(m\) consecutive
nimbers completely determine the next nimber. The mex of \(m\) numbers is at most \(m\), and there are only a finite number of sequences of length \(m\) whose
elements are natural numbers bounded by \(m\). Therefore, as the pile size
increases, the nimbers must eventually repeat.

We use this observation to find the period:

\begin{code}
subPeriod ds = go 0 mempty $ take big <$> subNimbers' ds where
  big = last ds
  go n m (h:t) = case mlookup h m of
    Just k -> (n, k)
    Nothing -> go (n + 1) (insert h n m) t

subPeriod [1,2,3]
subPeriod [2,5,6]
subPeriod [2,5,7]
\end{code}

The first number is the period, and the second number indicates the pile size
where the nimbers start cycling.

If \(\nimber{n} \ne \nimber{n - s}\) for all \(n\), then adding \(s\) to the
the subtraction set has no effect on the nimbers:

\begin{code}
subNimbers [2,5,6] 20
subNimbers [2,5,6,9] 20
subNimbers [2,5,6,9,13,16,17,20] 20
\end{code}

Are nimbers always purely periodic?

\begin{code}
subPeriod [2,4,7]
subNimbers [2,4,7] 32
\end{code}

The beginning of this nimber sequence never appears again.
Such examples seem rare.

*Ferguson's Pairing Property*: For any subtraction game,

\[
\nimber{n} = 1 \Leftrightarrow \nimber{n - s_1} = 0
\]

where \(s_1\) is the smallest member of the subtraction set.

We prove this by induction. The base cases are trivial.
Suppose \(\nimber{n} = 1\). If \(\nimber{n - s_1} \ne 0\), then \(\nimber{n -
s_1 - s} = 0\) for some \(s\) in the substraction set, so by inductive
hypothesis \(\nimber{n - s} = 1\), implying we can move from the nimber-1
position \(n\) to the nimber-1 position \(n - s\), a contradiction.

Conversely, suppose \(\nimber{n - s_1} = 0\), which immediately implies
\(\nimber{n} \gt 0\). If \(\nimber{n} \ne 1\) then \(\nimber{n - s} = 1\) for
some \(s\) in the subtraction set, so by inductive hypothesis
\(\nimber{n - s - s_1} = 0\), implying we can move from the nimber-0
position \(n - s_1\) to the nimber-0 position \(n - s_1 - s\), a contradiction.

This curious proof relies on the inductive hypothesis for the \((<=)\)
direction when proving \((=>)\) direction, and vice versa. It makes me
think of scissor lifts.

== Kayles ==

The game of Kayles starts with a row of evenly spaced bowling pins. On their
turn, a player bowls a ball that is the right size to either knock down one
pin, or two pins that were initially adjacent.

This is equivalent to a game with piles of coins where each turn, a player
takes one or two coins from a single pile, and optionally partitions any
remaining coins in the pile into two piles of any sizes.

\begin{code}
nextKayles = \case
  [] -> []
  n:nt -> (n:) <$> nextKayles nt <|> map ($ nt)
      [zcons a . zcons b | a <- [0..n - 1], b <- [n - 2 - a, n - 1 - a], a <= b]
  where
  zcons = \case
    0 -> id
    n -> (n:)

nextKayles [8]
\end{code}

We can figure out nimbers of positions with brute force:
    
\begin{code}
(`execState` mempty) $ mexM nextKayles [8]

do
  putStrLn "One-pile Kayles nimbers:"
  print $ filter ((<= 1) . length . fst) . toAscList .
    (`execState` mempty) $ mexM nextKayles [8]
\end{code}

But just as with Nim, a two-pile Kayles position is the sum of two one-pile
games, thus we can compute the nimber of a one-pile Kayles position more
efficiently with XOR. As before, our `go` helper adds the next nimber to the
front of the list, so we reverse the list before showing it.

\begin{code}
kayles = iterate go [0] where
  go ns@(_:nt) = (:ns) $
      mex $ zipWith xor ns (reverse ns) ++ zipWith xor nt (reverse nt)

reverse $ kayles !! 32
\end{code}

== Take-and-Break ==

We can combine and generalize the above ideas to describe a family of
take-and-break games identified by a list \(d_0, d_1, d_2, ...\) of bitstrings
where each set bit indicates a legal move. A position is a pile of coins,
and on their turn, and if bit \(b\) of \(d_k\) is set, then a player has the
option of taking \(k\) coins and partitioning the remaining coins into \(b\)
non-empty piles.

For example, if bit 89 of \(d_{64}\) is set, then one possible move is to take
64 coins from a pile and partition the remainder into 89 non-empty piles. This
implies there must be at least 153 coins in the pile.

Playing this game sounds tiresome, and at any rate, the thornier cases are
the smallest ones. For example:

  * If bit 0 of \(d_1\) is set and the others are unset, then if we take exactly one coin from a pile, we must leave a partition of size zero behind. Thus we can take exactly one coin from a pile only if it consists of exactly one coin.

  * If bit 1 of \(d_2\) is set and the others are unset, then if we take exactly two coins from a pile, we must leave a partition of size one behind. Thus we can take exactly two coins from a pile only if its size is greater than two.

  * If bits 0 and 1 of \(d_3\) are set and the others are unset, then if we
  take exactly three coins from a pile, we must leave a partition of size
  zero or one. Thus we can take exactly three coins from a pile as long as its
  size is at least three.

If we write a bitstring as a number, then [0,1,2,3] represents the game where a
player can choose one of the above three options on their move.

We require bits 0 and 1 of \(d_0\) to be unset. Setting bit 0 is pointless, as
this would mean taking nothing from a pile of coins and leaving nothing behind.
Setting bit 1 leads to a boring game, as it means we can take nothing from a
pile and leave a partition of size one, which is equivalent to passing provided
at least one pile exists.

Writing code to compute nimbers for these games is far easier than describing
them. We start with link:../haskell/count.html[a function that finds all
partitions of a number, described elsewhere in our notes]:

\begin{code}
party n k
  | k == 0 && n == 0 = [[]]
  | k <= 0 || n <= 0 = []
  | otherwise = ((++ [1]) <$> party (n - 1) (k - 1)) ++
    (map (+1) <$> party (n - k) k)

party 8 3
\end{code}

We represent bitstrings with numbers so we need a routine to decode them:

\begin{code}
bits = go 0 where
  go b n
    | n == 0 = []
    | otherwise = (if r == 1 then (b:) else id) $ go (succ b) q
    where (q, r) = divMod n 2

bits 257
\end{code}

Then the following calculates the nimbers of a take-and-break game:

\begin{code}
takeBreak ds = iterate go ([0], 1) where
  go (xs, n) = (nextNimber ds xs n : xs, succ n)

nextNimber ds acc n = mex $ map (foldr xor (0::Int) . map hist)
    $ concat $ zipWith partBits ds [0..n] where
  hist i = acc!!(n - 1 - i)
  partBits d k = party (n - k) =<< bits d
\end{code}

For example, the nimbers of the [0,1,2,3] game are:

\begin{code}
reverse . fst . (!!32) $ takeBreak [0, 1, 2, 3]
\end{code}

Berlekamp, Conway, and Guy write lists as a string. They assume each \(d_k\)
can be represented with a single character such as a hexadecimal digit, which
is reasonable for practical games. Then they concatenate the digits, with a
centered dot separating \(d_0\) from the others. If \(d_0 = 0\), then it is
omitted.

For example, the string *·123* means `[0,1,2,3]`, and *4·333...* means
`[4,3,3,3,...]`. At times, they use dots above digits to indicate repetition.

We change the format so it is easy to type. We replace the centered dot with
a standard period `(.)`, and a single `(#)` indicates the following digits are
to be repeated. Our simplistic parser performs no input validation.

\begin{code}
parse = ($ []) . \case
  '.':t -> (0:) . parse1 t
  d:'.':t -> (fromDigit d:) . parse1 t

parse1 = \case
  [] -> id
  '#':dt -> cycle . parse1 dt
  d:dt -> (fromDigit d:) . parse1 dt

fromDigit c
  | '0' <= c && c <= '9' = ord c - ord '0'
\end{code}

== Coding Quiz ==

Below, there is some wiggle room since different codes can refer to the
essentially the same game.

1. What is Nim's code?

2. What is the code of the `[2,5,6]` subtraction game?

3. What is Kayles' code?

4. Lasker's Nim is Nim plus one rule: instead of taking some coins, a player may break a pile into two non-empty piles. What is the code of this game?

5. In a game of Guiles, a player can remove piles of 1 or 2 coins completely, or take 2 coins from a pile and partition the remaining coins in the pile into two non-empty piles. What is its code?

6. Dawson's Chess is played on a 3x\(n\) chess board. Both White and Black have pawns of their colour on the rank closest to them, which move and capture as in standard chess. Captures are mandatory.
+
This game always ends in a stalemate where every file is empty, or two pawns blocking each other. Like Nim, but unlike chess, a player loses if there are no moves.
+
Although this appears to be a partisan game, it is in fact equivalent to an
impartial take-and-break game. What is its code?
+
This may help: Dawson's Kayles is a variant of Kayles where the only legal
move is to remove two pins that were initially adjacent.

7. Treblecross is like link:tictactoe.html[Tic-Tac-Toe] except both players use the same symbol X, and they play on one row which is at least 3 squares long. A player wins if they draw a third X in a row.
+
This game turns out to be equivalent to a take-and-break game. What is its code?

8. A game of Officers starts with an officer directly in command of \(n\)
soldiers. On their turn, a player picks an officer X with at least 4 direct
reports, and partitions them into two groups A and B where A must contain at
least 3 of the direct reports.
+
Then the player inserts an officer Y into the chain of command such that Y now
directly commands the members of A, while X now commands the members of B along
with Y. A player loses when this is impossible, that is, when every officer
directly commands 2 or 3 others.
+
The game turns out to be equivalent to a take-and-break game. What is its code?

The code below states the answers, and prints nimber cheat sheets for
each of these games. See Berlekamp, Conway, and Guy for the explanations.

\begin{code}
cheatSheet n = reverse . fst . (!!n) . takeBreak . parse

do
  let
    go s g = do
      putStrLn s
      print $ cheatSheet 25 g
  go "Nim" "0.#3"
  go "Subtract 2-5-6" ".030033"
  go "Lasker's Nim" "4.#3"
  go "Guiles" ".15"
  go "Kayles" ".77"
  go "Dawson's Chess" ".137"
  go "Dawson's Kayles" ".07"
  go "Treblecross" ".007"
  go "Officers" ".6"
\end{code}

== Breaking the code ==

We mention a few Nim-like games beyond the reach of our take-and-break coding
scheme.

*Grundy's Game*: On their turn, a player splits a pile into two unequal
non-empty piles. Hence:

\[
\newcommand{\mex}{\mathop{\rm mex}\nolimits}
\begin{gathered}
\nimber{0} = \nimber{1} = \nimber{2} = 0
\\
\nimber{n + 1} = \mex \{\nimber{n} \oplus \nimber{n-k} |
  k \in [1..\lfloor n / 2 \rfloor ] \}
\end{gathered}
\]

\begin{code}
grundyGrundy = iterate go [0,0,0] where
  go xs = (:xs) $ mex $ zipWith xor xs $
      take (div (length xs - 1) 2) $ tail $ reverse xs

reverse $ grundyGrundy !! 25
\end{code}

*Prim*: A player can remove \(m\) from a pile of \(n\) coins exactly when \(m\)
and \(n\) are coprime. There are two variants, depending on whether we allow a
pile size to go from 1 to 0.

The nimbers of piles of size 0 and 1 are immediate. Otherwise one can show
that if going from 1 to 0 is forbidden, the nimber of a pile of size \(n\) is
\(k\), where the \(k\)th prime is the smallest prime divisor of \(n\).
Furthermore, the nimber \(x\) of a position remains the same if going from 1 to
0 is legal, except when \(x\) is 0 or 1, in which case its nimber is \(1 - x\).

\begin{code}
primber flag n
  | n == 0 = 0
  | n == 1 = if flag then 1 else 0
  | flag = case k of
    0 -> 1
    1 -> 0
    _ -> k
  | otherwise = k
  where
  k = snd $ head $ dropWhile ((0 /=) . mod n . fst) $ zip primes [1..]

primes = sieve [2..] where
  sieve (p:x) = p : sieve [n | n <- x, n `mod` p /= 0]

primber False <$> [0..24]
primber True <$> [0..24]
\end{code}

*Dim*: A player can remove \(d\) from a pile of \(n\) coins exactly
when \(d\) divides \(n\). There are two variants, depending on whether we allow
\(d = n\).

One can show that the nimber of a pile of \(n\) coins is \(k\) where \(2^k\) is
the largest power of two dividing \(n\), provided taking all \(n\) coins is
illegal. If such a move is legal, then its nimber is \(k + 1\).

\begin{code}
dimber flag n
  | n == 0 = 0
  | flag = go n 1
  | otherwise = go n 0
  where
  go n k
    | r == 1 = k
    | otherwise = go q $ succ k
    where
    (q, r) = divMod n 2

dimber False <$> [0..24]
dimber True <$> [0..24]
\end{code}

== Demos ==

The code below reveals the connection between Treblecross and the
take-and-break game *·007*. We represent a position with a list of integers
indicating the location of the Xs, along with the total number of cells. We
assume no two Xs are adjacent, or have only one blank cell between them, where
it is obvious how to win.

\begin{code}
nimbers007 = cheatSheet 50 ".007"

wins ms = [(g, p) | (n, p) <- ms, let g = xor n h, n > g]
  where h = foldr xor 0 $ fst <$> ms

convertTreblecross ns sz = go 1 0 ns where
  go sides start = \case
    [] -> [(start, (sz - start, sides + 1))]
    n:nt -> (start, (n - start, sides)) : go 0 (n + 1) nt

winsTreblecross ns sz = uncurry findCut =<< wins ms where
  ms = go <$> convertTreblecross ns sz
  go p@(_, (len, sides)) = (nimbers007 !! (len + 2*(sides - 1)), p)

findCut tgt (start, (n, sides)) = case sides of
  0 -> go 2 2
  1 | start == 0 -> go 0 2
    | otherwise  -> go 2 0
  2 -> go 0 0
  where
  go r s = [start + a | a <- [r..n-1-s],
      tgt == xor (nimbers007!!(a-r)) (nimbers007!!(n-a-1-s))]

winsTreblecross [7, 10] 16
\end{code}

We do the same for Dawson's Chess, where we assume all obligatory pawn
captures have already been played. We use a one-off encoding scheme to
represent a file of the board:

----------------------------------------------------------------
. B B . . . B B
. W . B . W . B
. . W W W W . .
0 l 2 3 4 5 6 7
----------------------------------------------------------------

The `winsChess` function lists Black's winning moves, represented by the
index of the file where a pawn should be pushed.

\begin{code}
nimbers137 = cheatSheet 50 ".137"

findChess h (start, n)
  | n == 1 = [start]
  | n == 2 = [start, start + 1]
  | otherwise = ends ++ mids
  where
  ends = if nimbers137!!(n - 2) == h then [start, start + n - 1] else []
  mids = [start + i | i <- [1..n-2], (nimbers137!!(i-1)) `xor` (nimbers137!!(n-2-i)) == h]

convertChess = go id where
  go acc xs = case dropWhile ((2 /=) . snd) xs of
    [] -> acc []
    rest -> let
      (us, vs) = span ((2 ==) . snd) rest
      n = length us
      in go (acc . ((nimbers137!!n, (fst $ head us, n)):)) vs

winsChess = (uncurry findChess =<<) . wins . convertChess . zip [0..]

winsChess [0,1,0,2,2,2,0,3,0,0,2,2,2,2,0,3,0,1]
\end{code}

We demonstrate in a web widget. Here's Treblecross:

\begin{code}
data Treblecross = Treblecross
  { _board :: [Int]
  , _over :: Bool
  }

setTreblecross t = setGlobal . first (const t) =<< global
getTreblecross = fst <$> global

ins n = \case
  [] -> [n]
  xs@(x:xt)
    | x < n -> x : ins n xt
    | otherwise -> n : xs

rowSize = 30

newXXX = do
  setTreblecross $ Treblecross [] False
  jsEval_ $ "drawTreble(" ++ show rowSize ++ ");"
  jsEval_ $ "msg.innerHTML = '<br>';"
  jsEval_ $ "xxxCanvas.style.display = 'block';"
  jsEval_ $ "chessCanvas.style.display = 'none';"
  jsEval_ $ "howto.innerHTML = howtoXXX.innerHTML;"

treble ns start = all (`elem` ns) [start..start+2]

computerWin k = do
  ns <- _board <$> getTreblecross
  setTreblecross $ Treblecross (ins k ns) True
  jsEval_ $ "ecks(" ++ show k ++ ");"
  jsEval_ "msg.innerText = `Computer wins`;"

rand n = fromInteger . readInteger <$> jsEval ("Math.floor(Math.random() * " ++ show n ++ ");")

click :: Int -> Int -> IO ()
click x y = do
  g <- getTreblecross
  unless (_over g) $ do
    let n = div (x - 5) 20
    when (y >= 5 && y <= 25 && n >= 0 && n < rowSize) $ do
      ns <- _board <$> getTreblecross
      unless (elem n ns) do
        ns <- pure $ ins n ns
        jsEval_ $ "ecks(" ++ show n ++ ");"
        \cases
          | any (treble ns) [n-2..n] -> do
            setTreblecross $ Treblecross ns True
            jsEval_ $ "msg.innerText = `You win!`"
          | (n - 1) `elem` ns -> \cases
            | n >= 2 -> computerWin $ n - 2
            | otherwise -> computerWin $ n + 1
          | (n + 1) `elem` ns -> \cases
            | n >= 1 -> computerWin $ n - 1
            | otherwise -> computerWin $ n + 2
          | (n + 2) `elem` ns -> computerWin $ n + 1
          | (n - 2) `elem` ns -> computerWin $ n - 1
          | otherwise -> do
            let
              ks = case winsTreblecross ns rowSize of
                [] -> [0..rowSize - 1] \\ ns
                ks -> ks
            k <- (ks!!) <$> rand (length ks)
            setTreblecross $ Treblecross (ins k ns) False
            jsEval_ $ "ecks(" ++ show k ++ ");"
\end{code}

Here's Dawson's Chess, and some calls to kick things off.

\begin{code}
data DawsonChess = DawsonChess
  { _chess :: [Int]
  , _cursor :: Maybe (Int, [Int])
  , _winner :: Int
  }

setChess t = setGlobal . second (const t) =<< global
getChess = snd <$> global

checker c x y w h = jsEval_ $
    "checker(" ++ intercalate "," (show <$> [c, x, y, w, h]) ++ ")"

chsz = 40

drawCursor r c = do
  jsEval_ $ "cc.drawImage(fromCanvas," ++ show (c*chsz) ++ "," ++ show (r*chsz) ++ ");"

drawDest c = do
  jsEval_ $ "cc.drawImage(toCanvas," ++ show (c*chsz) ++ "," ++ show chsz ++ ");"
replace k v xs = us ++ (v:vs) where (us, _:vs) = splitAt k xs

captureBlack col pos = replace col (go $ pos!!col) pos where
  go = \case
    1 -> 7
    2 -> 4
    5 -> 3
    6 -> 0

captureWhite col pos = replace col (go $ pos!!col) pos where
  go = \case
    2 -> 6
    3 -> 5
    4 -> 0
    7 -> 1

chessComputer col pos
  | col > 0 && elem (pos!!pred col) [2,6] =
      captureBlack (pred col) $ captureBlack col pos
  | col < length pos - 1 && elem(pos!!succ col) [2,6] =
      captureBlack (succ col) $ captureBlack col pos
  | otherwise = case winsChess pos of
    [] -> let
      (us, _:vs) = break (== 2) pos
      in us ++ 3:vs
    ms -> replace (last ms) 3 pos

chessClick x y = do
  dc <- getChess
  when (_winner dc == 0) $ chessClick' dc x y

chessClick' dc x y = do
  let row = div y chsz
  let col = div x chsz
  let pos = _chess dc
  case _cursor dc of
    Just (srcCol, moves) -> do
      if row == 1 && elem col moves then do
          let
            stuck = all (`elem` [0,1,3])
            pos'
              | col == srcCol = replace col 1 pos
              | otherwise = captureWhite srcCol $ captureWhite col pos

          if stuck pos' then do
              setChess dc { _cursor = Nothing, _chess = pos', _winner = 1 }
              chessDraw pos'
              jsEval_ $ "msg.innerText = 'You win!'"
            else do
              let pos'' = chessComputer col pos'
              chessDraw pos''
              if stuck pos'' then do
                  jsEval_ $ "msg.innerText = 'Computer wins'"
                  setChess dc { _cursor = Nothing, _chess = pos'', _winner = -1 }
                else setChess dc { _cursor = Nothing, _chess = pos'' }
        else do
          setChess dc { _cursor = Nothing, _chess = pos }
          jsEval_ "unsnapChess();"
    Nothing -> do
      let x = pos!!col
      \cases
        | row == 1, elem x [1, 5] -> do
          setChess dc { _cursor = Just (col, []) }
          drawCursor 1 col
        | row == 2, elem x [2, 4] -> do
          let
            isCap (a, b) = elem a [2,4] && elem b [3,7]
            caps =
              [(k, k + 1) | (k, p) <- zip [0..] $ zip pos $ tail pos, isCap p] ++
              [(k, k - 1) | (k, p) <- zip [1..] $ zip (tail pos) pos, isCap p]
            moves = case caps of
              [] -> [col]
              _ -> snd <$> filter ((col == ) . fst) caps
          setChess dc { _cursor = Just (col, moves) }
          mapM_ drawDest moves
          drawCursor 2 col
        | row == 2, elem x [3, 5] -> do
          setChess dc { _cursor = Just (col, []) }
          drawCursor 2 col
        _ -> pure ()

drawFile col = \case
  0 -> pure ()
  1 -> wh [1] *> bl [0]
  2 -> wh [2] *> bl [0]
  3 -> wh [2] *> bl [1]
  4 -> wh [2]
  5 -> wh [1,2]
  6 -> bl [0]
  7 -> bl [0,1]
  where
  x = col*chsz + 2
  wh = mapM_ \w -> jsEval_ $ "pawn(`\\u2659`," ++ show x ++ ", " ++ show (w*chsz + 35) ++ ");"
  bl = mapM_ \b -> jsEval_ $ "pawn(`\\u265f`," ++ show x ++ ", " ++ show (b*chsz + 35) ++ ");"

chessDraw pos = do
  sequence [checker (mod (x + y) 2) (x*chsz) (y*chsz) chsz chsz
      | y <- [0..2], x <- [0..length pos - 1]]
  sequence $ zipWith drawFile [0..] pos
  jsEval_ "snapChess();"

newChess = do
  let pos = replicate 16 2
  setChess $ DawsonChess pos Nothing 0
  chessDraw pos
  jsEval_ $ "xxxCanvas.style.display = 'none';"
  jsEval_ $ "chessCanvas.style.display = 'block';"
  jsEval_ $ "howto.innerHTML = howtoChess.innerHTML;"

do
  setGlobal (undefined, undefined)
  jsEval_ "initXXX(repl);"
  jsEval $ "initChess(" ++ show chsz ++ ", 5);"
  newChess
\end{code}
