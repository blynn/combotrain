= Summer Games =

Let's recap link:nim.html[our notes on the game of Nim].

Every position \(X\) of any impartial game can be mapped to a natural number
\(\newcommand{\nimber}[1]{\mathcal{G}(#1)}\nimber{X}\) known as its nimber, or
Grundy value. Nimbers can be defined recursively on the game tree:

  * The nimber of each leaf is zero.

  * The nimber of an internal node is the mex of the nimbers of its children.

The following `mexM` function computes nimbers according to the above, using
memoization to avoid duplicating work:

\begin{code}
jsEval "curl_module('../compiler/Map.ob')"
\end{code}

\begin{code}
mex :: [Int] -> Int
mex xs = head $ [0..] \\ xs

import Map

mexM f n = do
  mlookup n <$> get >>= \case
    Just m -> pure m
    Nothing -> do
      m <- case f n of
        [] -> pure 0
        ks -> mex <$> mapM (mexM f) ks
      modify $ insert n m
      pure m
\end{code}

In general, this is little better than minimax search, which also discovers all
winning positions. However, nimbers allow shortcuts if we can decompose a game
into a sum of two games \(G + H\), for the nimber of the sum of positions is
the XOR of the nimbers of each summand. In particular, the nimber of any Nim
position just the XOR of the sizes of each pile.

We can go the other way and use nimbers and XOR to play optimally in a
Frankenstein game of, say, Nim plus Kayles plus Treblecross, that is, we start
the three games simultaneously and a turn consists of making a move in one of
the three positions; a player loses if unable to move in any of the three.

https://en.wikipedia.org/wiki/Winning_Ways_for_Your_Mathematical_Plays[Berlekamp,
Conway, and Guy, _Winning Ways for Your Mathematical Plays_], goes beyond Nim
and analyzes games with shortcuts based on game sums. While we cannot compute
their nimbers as efficiently as we can for Nim, we can compute enough to
trounce an uninitiated player for realistic game sizes.

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

We prove this by induction.
Suppose \(\nimber{n} = 1\). If \(\nimber{n - s_1} \ne 0\), then \(n - s_1\) is
a losing position. Thus \(\nimber{n - s_1 - s} = 0\) for some \(s\) in the
substraction set, so by inductive hypothesis \(\nimber{n - s} = 1\), implying
we can move from a nimber 1 position to a nimber 1 position, a contradiction.

Conversely, suppose \(\nimber{n - s_1} = 0\), which immediately implies
\(\nimber{n} \gt 0\). If \(\nimber{n} \ne 1\) then \(\nimber{n - s} = 1\) for
some \(s\) in the subtraction set, so by inductive hypothesis
\(\nimber{n - s - s_1} = 0\), implying
we can move from a nimber 0 position to a nimber 0 position, a contradiction.

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
front of the list, so they appear in reverse order.

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
this would mean taking nothing from a pile of coins and leaving nothing behind,
which is impossible (unless we allow piles of size zero, but even then the
analysis is trivial). Setting bit 1 leads to a boring game, as it means we can
take nothing from a pile and leave a partition of size one, which is equivalent
to passing provided at least one pile exists.

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

Berlekamp, Conway, and Guy write the code of a take-and-break game as a string.
They assume each number can be represented with a single character such as a
hexadecimal digit, which is reasonable for practical games. Then they
concatenate the digits, with a centered dot separating \(d_0\) from the others.
If \(d_0 = 0\), then it is omitted. For example, the string *·123* means
`[0,1,2,3]`, and *4·333...* means `[4,3,3,3,...]`. At time, they use dots
above digits to indicate repetition.

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

7. Treblecross is like link:tictactoe.html[Tic-Tac-Toe] except the board measures 1 by \(n\) and both players use the same symbol X. A player wins if they draw a third X in a row, or if the other player has no moves (which can only happen for \(n < 3\)).
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

== Grundy's Game ==

Grundy's Game is another game played with piles of coins. On their turn,
a player splits a pile into two unequal non-empty piles. Hence the game ends
when all piles have size one or two.

The inequality check is beyond the scope of our take-and-break coding scheme,
but we can still compute Grundy values of Grundy's Game easily enough via
game sums.

\[
\newcommand{\mex}{\mathop{\rm mex}\nolimits}
\nimber{n + 1} = \mex \{\nimber{n} \oplus \nimber{n-k} |
  k \in [1..\lfloor n / 2 \rfloor ] \}
\]

\begin{code}
grundyGrundy = iterate go [0,0,0] where
  go xs = (:xs) $ mex $ zipWith xor xs $
      take (div (length xs - 1) 2) $ tail $ reverse xs

reverse $ grundyGrundy !! 25
\end{code}

== Prim ==

In this game, a player can remove \(m\) from a pile of \(n\) coins exactly
when \(m\) and \(n\) are coprime. There are two variants, depending on whether
we allow a pile size to go from 1 to 0.

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

== Dim ==

In this game, a player can remove \(d\) from a pile of \(n\) coins exactly
when \(d\) divides \(n\). There are two variants, depending on whether
we allow \(d = n\).

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
