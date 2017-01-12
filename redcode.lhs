= Core War =

In a round of https://en.wikipedia.org/wiki/Core_War[Core War], two
programs attempt to halt each other by overwriting instructions that are
about to be executed. Watch a battle between two famous warriors,
CHANG1 (left, blue) versus MICE (right, red):

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src="redcode.js"></script>
<canvas id="canvas" style="border: 1px solid black;" width="300" height="240">
</canvas>
<p><textarea id="con" rows="5" cols="80" readonly></textarea></p>
<p><button id="goB">Restart</button>
<button id="stopB">Halt</button></p>
<p><textarea id="player1" rows="20" cols="16" spellcheck="false">
jmp 4
mov 2, -1
jmp -1
dat 9
spl -2
spl 4
add #-16, -3
mov -4, @-4
jmp -4
spl 2
jmp -1
mov 0 1
</textarea>
<textarea id="player2" rows="20" cols="16" spellcheck="false">
jmp 2
dat 0
mov #12, -1
mov @-2, <5
djn -1, -3
spl @3
add #653, 2
jmz -5, -6
dat 833
</textarea></p>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//////////////////////////////////////////////////////////////////////////////
Dwarf

  add #4, 3
  mov 2, @2
  jmp -2
  dat 0

Gemini

  jmp 3
  dat 0
  dat 99
  mov @-2, @-1
  sne -3, #9
  jmp 4
  add #1, -5
  add #1, -5
  jmp -5
  mov #99, 93
  jmp 93
//////////////////////////////////////////////////////////////////////////////

The programs are written in a language called Redcode. For details, read the
original http://www.koth.org/info/akdewdney/['Scientific American' articles]
introducing the game, as well as
http://vyznev.net/corewar/guide.html[a guide to the 1994 revision of Redcode],
which is nicer than http://corewar.co.uk/standards/icws94.htm[the official document]. See also
http://www.koth.org/planar/by-name/complete.htm[complete listings of many
programs].

Some potential points of confusion:

 - In the original Redcode, MOV with an immediate A field writes a DAT
   instruction to the target address. In later versions, it overwrites the B
   field only by default.
 - The instruction encoding scheme given in the original article is irrelevant.
   For example, the only way to change an instruction is to use MOV to copy
   another instruction,
 - The '94 specification defines CMP to be an alias of SEQ, but
   http://www.koth.org/info/akdewdney/images/Gemini.jpg[the Gemini program
   featured in the original article], it clearly means SNE.
 - In general, documentation felt buggy. For example, I happened to
   browse http://www.koth.org/planar/rc/theMystery1.5.txt[the source of
   theMystery 1.5], which claims "spl 1; mov -1, 0; mov -1, 0" makes 7
   processes. It seems it results in 5 processes. To get 7, we could write
   "spl 1; spl 1; mov -1, 0".

== The Journey to MARS ==

The above Memory Array Redcode Simulator was written in Haskell and
compiled to JavaScript with http://haste-lang.org/[Haste].

To build this webpage, install http://haste-lang.org/[Haste] and
http://asciidoc.org/[AsciiDoc], then run:

------------------------------------------------------------------------------
$ haste-cabal install parsec
$ wget http://cs.stanford.edu/~blynn/haskell/redcode.lhs
$ hastec redcode.lhs
$ sed 's/^\\.*{code}$/-----/' redcode.lhs | asciidoc -o - - > redcode.html
------------------------------------------------------------------------------

We start with imports for our Redcode emulator:

\begin{code}
{-# LANGUAGE ViewPatterns #-}
import Control.Concurrent.MVar
import Control.Monad
import Data.Char
import Data.Sequence (Seq, viewl, ViewL(..), (><))
import qualified Data.Sequence as Seq
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
\end{code}

Then append some Haste-specific imports:

\begin{code}
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
\end{code}

Arrays are cumbersome in Haskell because of purity, so we use `Data.Map` to
represent the memory array of 8000 cells, initialized to `DAT 0` instructions.
The game state consists of the memory array, along with a tuple holding a
program ID along with the program counters of its threads. For the latter, we
use `Data.Sequence` instead of a list to obtain fast and strict  queue
operations.

\begin{code}
type Arg = (Char, Int)
data Op = Op String Arg Arg deriving (Show, Eq)
type Core = Map Int Op
data Game = Game Core [(Int, Seq Int)] deriving Show

sz = 8000

initCore = M.fromList $ zip [0..sz - 1] $ repeat $ Op "DAT" ('#', 0) ('#', 0)
\end{code}

Simulating a single instruction at a given location results in a list of
changes to be made to memory, and a list of the next locations to execute.

Focusing on the changes makes it easy to update our visualization of memory. If
we instead returned a new map, we might have to redraw the entire screen to
show the next state.

I began with the three original memory addressing modes:

\begin{code}
inskvs = foldl' (\c (k, v) -> M.insert k v c)

load ops a c = inskvs c $ zip [a..] ops

exeRedcode c ip = f op ma mb where
  Op op (ma, a) (mb, b) = c!ip
  f "DAT" _   _ = ([], [])
  f "NOP" _   _ = ([], adv)
  f "MOV" '#' _ = ([(rb, putB aa ib)], adv)
  f "MOV" _ '#' = ([(rb, putB ba ib)], adv)
  f "MOV" _   _ = ([(rb, c!ra)], adv)
  f "SEQ" '#' _ = skipIf $ aa == bb
  f "SEQ" _ '#' = skipIf $ ba == bb
  f "SEQ" _   _ = skipIf $ ia == ib
  f "SNE" '#' _ = skipIf $ aa /= bb
  f "SNE" _ '#' = skipIf $ ba /= bb
  f "SNE" _   _ = skipIf $ ia /= ib
  f "ADD" '#' _ = ([(rb, putB (add a $ bb) ib)], adv)
  f "ADD" _ '#' = ([(rb, putB (add ba $ bb) ib)], adv)
  f "ADD" _   _ = ([(rb, putA (add aa $ ab) $ putB (add ba $ bb) ib)], adv)
  f "SPL" _   _ = ([], adv ++ [ra])
  f "JMP" _   _ = jumpIf True      ra
  f "JMN" _   _ = jumpIf (bb /= 0) ra
  f "JMZ" _   _ = jumpIf (bb == 0) ra
  f "DJN" _   _ = effect [(rb, putB (sub bb 1) ib)] $ jumpIf (bb /= 1) ra
  f "DJZ" _   _ = effect [(rb, putB (sub bb 1) ib)] $ jumpIf (bb == 1) ra
  f op _ _ = error $ "huh " ++ op
  jumpIf True  a = ([], [a])
  jumpIf False _ = ([], adv)
  skipIf True  = ([], map (add 1) $ adv)
  skipIf False = ([], adv)
  effect es (ds, a) = (ds ++ es, a)
  ra = resolve c ip (ma, a)
  rb = resolve c ip (mb, b)
  ia = c!ra
  ib = c!rb
  aa = getA ia
  ba = getB ia
  ab = getA ib
  bb = getB ib
  adv = [add ip 1]

getA (Op _ (_, a) _) = a
getB (Op _ _ (_, b)) = b
putA a (Op op (m, _) mb) = Op op (m, a) mb
putB b (Op op ma (m, _)) = Op op ma (m, b)

add x y = (x + y) `mod` sz
sub x y = (x + sz - y) `mod` sz

resolve c ip ('#', i) = ip
resolve c ip ('$', i) = add ip i
resolve c ip ('@', i) = let j = add ip i in add j $ getB $ c!j
\end{code}

Later I learned of newer addressing modes that predecrement or postincrement.
I hastily added a wrapper function to handle the case needed for the MICE
program:

\begin{code}
resolve c ip ('<', i) = resolve c ip ('@', i)

exe c ip = (preb ++ prea ++ deltas, ip1) where
  Op _ (ma, a) (mb, b) = c!ip
  preb | mb == '<' = [(rb, putB (sub (getB $ c!rb) 1) $ c!rb)]
       | otherwise = []
  cb = inskvs c preb
  rb = resolve c ip ('$', b)
  prea | ma == '<' = [(ra, putB (sub (getB $ cb!ra) 1) $ cb!ra)]
       | otherwise = []
  ca = inskvs cb prea
  ra = resolve cb ip ('$', a)
  (deltas, ip1) = exeRedcode ca ip
\end{code}

I had no motivation to add the other addressing modes.

Let's move on to the assembler. We use the Parsec parser combinator library:

\begin{code}
num :: Parser Int
num = do
  s <- option id $ const negate <$> char '-'
  s . read <$> many1 digit

arg = do
  spaces
  m <- option '$' $ oneOf "@#$<"
  n <- num
  return (m, standardize n)

standardize n | m < 0     = sz - m
              | otherwise = m
              where m = mod n sz

jumps = words "JMP JMZ JMN DJZ DJN SPL"
known = flip S.member $ S.fromList $ words "MOV ADD SUB SEQ SNE DAT " ++ jumps
isJump = flip S.member $ S.fromList jumps

unalias "CMP" = "SNE"
unalias "JMG" = "JMN"
unalias s     = s

asm :: Parser Op
asm = do
  spaces
  op <- unalias . map toUpper <$> many1 letter
  if not $ known op then fail $ "unknown: " ++ op else do
    a <- arg
    m <- optionMaybe $ optional (try $ spaces >> char ',') >> arg
    spaces
    eof
    case m of
      Just b -> return $ Op op a b
      Nothing -> if isJump op then return $ Op op a ('#', 0)
        else if op == "DAT"  then return $ Op op ('#', 0) a
        else fail $ "needs 2 args: " ++ op
\end{code}

Lastly, we add a GUI. We have a timer that fires every 16 milliseconds, which
causes our program to advance the game held in an MVar by 64 steps. Each of the
two warriors is limited to 32 processes.

I tried using a once-only timer that would set up the next once-only timer,
which would then be canceled if the simulation were halted, but I couldn't
get `stopTimer` to work.

We use an `MVar` to store the game state between ticks. An `IORef` would work
too, since JavaScript is single-threaded.

I spent little effort on this part. The code here is tightly coupled to Haste
and HTML: rewriting it for, say, SDL or GHCJS would require big changes anyway.

\begin{code}
passive = [RGB 63 63 191, RGB 191 63 63]
active = [RGB 127 127 255, RGB 255 127 127]

main = withElems ["canvas", "player1", "player2", "con", "goB", "stopB"] $
     \[canvasE, player1E, player2E, conE, goB, stopB] -> do
  Just canvas <- fromElem canvasE
  gv <- newMVar Nothing
  let
    mark c a = renderOnTop canvas $ color c $ box x y
      where (y, x) = divMod a 100
    box x y = fill $ rect (xf, yf) (xf + 3, yf + 3) where
      xf = fromIntegral x * 3
      yf = fromIntegral y * 3
    tryStep = do
      jg <- takeMVar gv
      case jg of
        Just g -> step g
        Nothing -> putMVar gv Nothing
    con s = do
      v0 <- getProp conE "value"
      setProp conE "value" $ v0 ++ s ++ "\n"

    step g@(Game c []) = putMVar gv Nothing >> con "all programs halted"

    step g@(Game c ((id, viewl -> ip :< rest):players)) = do
      let
        (deltas, next) = exe c ip
        truncNext = take (32 - Seq.length rest) $ next
        ipq = rest >< Seq.fromList truncNext
        c1 = inskvs c deltas
      mapM (mark (passive!!id) . fst) deltas
      mark (passive!!id) ip
      mapM (mark (active!!id)) truncNext
      case viewl ipq of
        EmptyL -> do
          con $ "program " ++ show id ++ " halted"
          putMVar gv $ Just $ Game c1 players
        _ -> putMVar gv $ Just $ Game c1 $ players ++ [(id, ipq)]

    newMatch = do
      render canvas $ color (RGB 0 0 0) $ fill $ rect (0, 0) (300, 240)
      setProp conE "value" $ "new match: 0 vs 1\n"
      s <- getProp player1E "value"
      case mapM (parse asm "") $ lines s of
        Left err -> do
          swapMVar gv Nothing
          con $ show err
        Right p1 -> do
          s <- getProp player2E "value"
          case mapM (parse asm "") $ lines s of
            Left err -> do
              swapMVar gv Nothing
              con $ show err
            Right p2 -> gameOn p1 p2

    gameOn p1 p2 = do
      mapM_ (mark $ passive!!0) [0..length p1 - 1]
      mark (active!!0) 0
      mapM_ (mark $ passive!!1) [4000..4000 + length p2 - 1]
      mark (active!!1) 4000
      void $ swapMVar gv $ Just $ Game (load p2 4000 $ load p1 0 initCore)
        [(0, Seq.singleton 0), (1, Seq.singleton 4000)]
      con $ "running programs"

  void $ goB `onEvent` Click $ \_ -> newMatch

  void $ stopB `onEvent` Click $ \_ -> do
    jg <- takeMVar gv
    case jg of
      Just _ -> con "match halted"
      Nothing -> pure ()
    putMVar gv Nothing

  newMatch
  void $ setTimer (Repeat 16) $ replicateM_ 64 tryStep
\end{code}
