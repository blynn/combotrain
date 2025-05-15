= BASIC games =

"It is practically impossible to teach good programming to students that have
had a prior exposure to BASIC: as potential programmers they are mentally
mutilated beyond hope of regeneration."
-- https://www.cs.utexas.edu/~EWD/transcriptions/EWD04xx/EWD498.html[Edsger W. Dijkstra]

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<link href="https://fonts.googleapis.com/css2?family=Ubuntu+Mono:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet">

<style>
#tty { font-family: 'Ubuntu Mono', monospace; font-size:100%;
  color: darkgrey;
  background: lightgrey;
}
#tty:focus {
  color: black;
  background: white;
}
</style>
<div id="buttons"></div>
<div><textarea rows="40" cols="80" id="tty" readonly></textarea></div>
<script>"use strict";
let reading = false;
let buf;
let run;
function inp(repl) {
  buf = "";
  tty.value += "? █";
  reading = true;
}
function out(s) {
  tty.value += s;
  tty.scrollTop = tty.scrollHeight;
}
tty.addEventListener("keydown", ev => {
  ev.preventDefault();
  if (ev.keyCode == 27) {
    tty.blur();
    return;
  }
  if (!reading) return;
  if (ev.keyCode >= 32 && ev.keyCode < 127) {
    const c = String.fromCharCode(ev.keyCode);
    buf += c;
    tty.value = tty.value.slice(0, -1) + c + "█";
  } else switch(ev.keyCode) {
  case 8:
    if (buf != "") {
      buf = buf.slice(0, -1);
      tty.value = tty.value.slice(0, -2) + "█";
    }
    break;
  case 13:
    tty.value = tty.value.slice(0, -1) + "\n";
    reading = false;
    run('glow "' + buf + '"');
    break;
  }
});
function reset(repl) {
  reading = false;
  tty.value = "";
  buf = "";
  repl.postRun = ()=>{};
}
function initDemo(repl) {
  run = function(s) { repl.run("chat", ["Main"], s); };
  function addButton(name, s) {
    const b = document.createElement("button");
    b.innerText = name;
    buttons.appendChild(b);
    b.addEventListener("click", ev => { reset(repl); run(s); tty.focus(); });
  }
  addButton("Reverse", "runBASIC reverseGame");
  addButton("Wumpus", "runBASIC wumpus");
  addButton("Bagels", "runBASIC bagels");
}
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

I have many fond childhood memories of my first programming language: BASIC.

Many of them revolve around how hard it was to find source code to study,
especially for a penniless kid. I was so desperate that I once memorized a
snippet of BASIC in some computer magazine at a newsagent's shop, even though I
had no idea what it did.

I occasionally unearthed gems at my local library like
https://usborne.com/us/books/computer-and-coding-books[Usborne computer
programming books], but otherwise, I only had an eclectic collection of books
so small in number that I can easily remember each one of them.

Among my most treasured was https://www.atariarchives.org/morebasicgames/[_More
BASIC Computer Games_], edited by David H. Ahl. I lacked
https://www.atariarchives.org/basicgames/[its prequel], but I did have
https://archive.org/details/Whattodoafteryouhitreturn[_What To Do After You Hit
Return_] by People's Computer Company, whose contents overlap.

I rarely tried any of the programs. Rather, I'd read those books for ideas. My
computer could do graphics and sound. Why limit myself to clunky text games?
Give me sound and fury! In any case, the books would allude to a mysterious
"teletype". I had no easy way of discovering what this meant at the time, so I
wondered if the games would work properly on my computer.

Now I know. Once upon a time, instead of monitors, they had a cross between a
printer and a typewriter (I'm old enough to know have encountered these
things!), so that the human and computer wind up typing on the same paper.
I'm yet to compute with a real teletype, but it sounds cool: you get the whole
transcript of the collaboration between human nad computer. There's no 25-row
limit. No power is needed for the display, and it's easy on the eyes.

Indulging in nostalgia, I ported some of the games from the above books to
Haskell, using a `textarea` element with a scrollbar to approximate a teletype.

== Extended Memory ==

While I'm at it, I should mention
https://vtda.org/books/Computing/Programming/BASIC/100%20Ready-To-Run%20Programs%20and%20Subroutines%20for%20the%20IBM%20PC.pdf[_100
Ready-To-Run Programs and Subroutines for the IBM PC_], by Jeff Bretz and John
Clark Craig. I possessed a translation of this book which I couldn't read, but
I could type in the programs just fine, and it was nice to have code that
targeted my machine for a change.

I bought (or more accurately, got my parents to buy) a book by Steve Lucas,
https://download.file-hunter.com/Books/EN/MSX%20Adventure%20Programming.pdf[_MSX
Adventure Programming_]. I had no idea what MSX meant, but I could see that
their dialect of BASIC was similar if not the same as GW-BASIC. I tried to type
in some of the programs, but I lost patience.

Mum would bring home old issues of
https://en.wikipedia.org/wiki/APC_(magazine)[_Australian Personal Computer_]
from work. In those days they had program listings, but the BASIC programs were
rarely games. Some were programs that wrote bytes to a `.COM` file so they were
really hex dumps. Nonetheless, ultimately I did learn a lot from this magazine.

Towards the end of primary school, I acquired a few periodicals.
link:../asm/boot.html[A _Computing Now!_ magazine] containing a tutorial on
programming the boot sector, an issue of
https://en.wikipedia.org/wiki/Dr.%5fDobb%27s%5fJournal[_Dr. Dobb's Journal_],
and multiple(!) issues of a UK magazine which appears to be known as
https://www.computinghistory.org.uk/det/24627/Personal-Computing-with-the-Amstrad-May-1990/[_Personal
Computing with the Amstrad_], though by my time, there was no trace of this
mythical Amstrad. All I saw was a large italicized "PC" emblazoned on the
cover, with the reassuring "IBM and compatible PCs" in smaller print.

The last of these were my favourite. They were the only ones which I recall
physically buying myself, that is, picking up the magazine off the rack and
handing coins to the proprietor. I learned assembly programming from them,
which opened up new worlds. And each issue was bundled with a 5¼-inch floppy
disk full of DOS programs.

I recall `PIANOLA.COM`, which played up to 4-notes simultaneously through the
PC speaker thanks to special tricks invovling the 8253 chip while animating
something resembling a piano roll; I believe one sample tune was "I Do Like to
Be Beside the Seaside". Another made a sound effect that was advertised to
sound a bit like the TARDIS warping. Another had software that could be
unlocked by typing the right key. I tried reverse engineering it with a
disassembler, unsuccessfully.

== Let it flow ==

link:index.html[Previously, we passed continuations around for control flow].
This time, we'd like to translate BASIC programs to something like:

----------------------------------------------------------------
do
  a <- input  -- INPUT $A
  print a     -- PRINT $A
----------------------------------------------------------------

This turns out to be a little complicated. Evaluating `input` calls a
JavaScript function that prepares HTML elements for input, but it returns
before the input is ready. Another JavaScript function is eventually called
when the input is ready to be passed back to our program, asynchronously.

We reconcile this control flow mismtach via our compiler's `global` feature: we
define a monad whose bind method stores a continuation that we expect the
JavaScript to later call. Some care is needed since calls from JavaScript to
our Haskell compiler are not re-entrant. At times, we rather than call
JavaScript from Haskell, we set up a call that is invoked after our interpreter
has yielded control.

The crudeness of our approach shows up in the types. The type argument of
`BASIC` is unused. However, it works well enough for our simple programs, and
we only need a few impure functions anyway: `INPUT`; `PRINT`; `RND`.
Since `print` is in the Haskell Prelude, we use `output` to print strings.

\begin{code}
data BASIC a = BASIC { unBASIC :: IO () }

runBASIC m = setGlobal [const $ BASIC (pure () :: IO ())] *> unBASIC m

glow x = do
  f:fs <- global
  setGlobal fs
  unBASIC $ f x

pushGlow x = global >>= setGlobal . (x:)

jsYield s = jsEval_ $ "repl.postRun = () => { " ++ s ++ " repl.postRun = () => {}; };"

instance Functor BASIC where
  fmap f x = x >>= return . f
instance Applicative BASIC where
  pure = return
  f <*> x = f >>= (<$> x)
instance Monad BASIC where
  return = BASIC . glow
  x >>= f = BASIC $ pushGlow f *> unBASIC x

input = BASIC $ jsYield "inp(repl);"
output s = BASIC $ jsEval_ ("out(`" ++ s ++ "\n`);") *> glow ()
outputn s = BASIC $ jsEval_ ("out(`" ++ s ++ "`);") *> glow ()
outputs ss = mapM_ id $ output <$> ss

rand :: Ring a => a -> BASIC Integer
rand n = BASIC $ glow . readInteger =<< jsEval ("Math.floor(Math.random() * " ++ show n ++ ");")
\end{code}

We add a helper that is much like `whileM` from `Control.Monad.Loops`.

\begin{code}
whilishM :: Monad m => (a -> Bool) -> m a -> m a
whilishM cond f = go where
  go = do
    x <- f
    if cond x then go else pure x
\end{code}

== Reverse ==

First up is
https://www.atariarchives.org/basicgames/showpage.php?page=136[Reverse,
from BASIC Games].

Naturally, we use a list instead of an array. The main play loop is trivial
thanks to functions like `splitAt` and `reverse`, and the remaining only
non-trivial routine is the shuffling of a list. We mimic the rejection
sampling algorithm of the original.

\begin{code}
n = 9

rules = outputs
  [ "", "THIS IS THE GAME OF 'REVERSE'.  TO WIN, ALL YOU HAVE"
  , "TO DO IS ARRANGE A LIST OF NUMBERS (1 THROUGH " ++ show n ++ ")"
  , "IN NUMERICAL ORDER FROM LEFT TO RIGHT. TO MOVE, YOU"
  , "TELL ME HOW MANY NUMBERS (COUNTING FROM THE LEFT) TO"
  , "REVERSE.  FOR EXAMPLE, IF THE CURRENT LIST IS:"
  , "", "2 3 4 5 1 6 7 8 9"
  , "", "AND YOU REVERSE 4, THE RESULT WILL BE:"
  , "", "5 4 3 2 1 6 7 8 9"
  , "", "NOW IF YOU REVERSE 5, YOU WIN!"
  , "", "1 2 3 4 5 6 7 8 9", ""
  , "NO DOUBT YOU WILL LIKE THIS GAME, BUT"
  , "IF YOU WANT TO QUIT, REVERSE 0 (ZERO).", ""
  ]

printList a = outputs ["", unwords $ (' ':) . show <$> a, ""]

reverseGame = do
  outputs
    [ replicate 31 ' ' ++ "REVERSE"
    , replicate 14 ' ' ++ "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
    , "", "", "", "REVERSE -- A GAME OF SKILL", ""]
  outputn "DO YOU WANT THE RULES"
  s <- input
  when (s /= "NO") rules
  play

play = do
  outputs ["", "HERE WE GO ... THE LIST IS:"]
  h <- (2 +) <$> rand (n - 1)
  let
    mkList acc k
      | k == 0 = pure acc
      | otherwise = do
        a <- succ <$> rand n
        if a `elem` acc then mkList acc k else mkList (a:acc) (k - 1)
  a <- reverse <$> mkList [h] (n - 1)
  printList a
  turn a 1

turn a t = do
  outputn "HOW MANY SHALL I REVERSE"
  r <- fromInteger . readInteger <$> input
  \cases
    | r == 0 -> askTryAgain
    | r <= n -> do
      let 
        (as, bs) = splitAt r a
        a' = reverse as ++ bs
      printList a'
      if a' == [1..n] then do
          outputs ["YOU WON IT IN " ++ show t ++ " MOVES!!!", ""]
          askTryAgain
        else turn a' $ succ t
    _ -> do
      output $ "OOPS!  TOO MANY!  I CAN REVERSE AT MOST " ++ show n
      turn a t

askTryAgain = do
  outputn "\nTRY AGAIN (YES OR NO)"
  a <- input
  if a == "YES" then play else outputs ["", "O.K.  HOPE YOU HAD FUN!!"]
\end{code}

== Wumpus ==

Porting
https://www.atariarchives.org/morebasicgames/showpage.php?page=178[Wumpus] to
Haskell was tedious at times, but also illuminating, as we're forced to tease
out the variables that each function touches. One of Dijkstra's complaints
about BASIC surely must have been that the program state ends up like a
hoarder's bedroom.

This perhaps is not Dijkstra's biggest gripe, because he is well-known for
https://homepages.cwi.nl/~storm/teaching/reader/Dijkstra68.pdf[criticizing
the GOTO statement] which are plentiful in the above books. (By the way,
Dijkstra originally titled his letter "A Case Against the Goto Statement";
Niklaus Wirth changed it to the more damning
https://en.wikipedia.org/wiki/Considered_harmful["Go To Statement Considered
Harmful"].)

But at least for this program, once the global state was sorted out, it was
easy to convert GOTOs to function calls. GOSUB and RETURN was tougher; they
may be more principled, but they are hard to follow by eye without indentation!

\begin{code}
s = [ [2,5,8], [1,3,10], [2,4,12], [3,5,14], [1,4,6]
    , [5,7,15], [6,8,17], [1,7,9], [8,10,18], [2,9,11]
    , [10,12,19], [3,11,13], [12,14,20], [4,13,15], [6,14,16]
    , [15,17,20], [7,16,18], [9,17,19], [11,18,20], [13,16,19]
    ]

dsts n = s !! pred n

wumpus = do
  outputs
    [ replicate 32 ' ' ++ "WUMPUS"
    , replicate 14 ' ' ++ "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
    , "", "", ""]
  outputn "INSTRUCTIONS (Y-N)"
  i <- input
  unless (i == "N") $ outputs
    [ "WELCOME TO 'HUNT THE WUMPUS'"
    , "  THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM"
    , "HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A"
    , "DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW"
    , "WHAT A DODECAHEDRON IS, ASK SOMEONE)"
    , ""
    , "     HAZARDS:"
    , "BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM"
    , "     IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)"
    , "SUPERBATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU"
    , "     GO THERE, A BAT GRABS YOU AND TAKE YOU TO SOME OTHER"
    , "     ROOM AT RANDOM. (WHICH MIGHT BE TROUBLESOME)"
    , ""
    , "     WUMPUS:"
    , "THE WUMPUS IS NOT BOTHERED BY THE HAZARDS (HE HAS SUCKER"
    , "FEET AND IS TOO BIG FOR A BAT TO LIFT). USUALLY"
    , "HE IS ASLEEP. TWO THINGS THAT WAKE HIM UP: YOUR ENTERING"
    , "HIS ROOM OR YOUR SHOOTING AN ARROW."
    , "     IF THE WUMPUS WAKES, HE MOVES (P=.75) ONE ROOM"
    , "OR STAYS STILL (P=.25). AFTER THAT, IF HE IS WHERE YOU"
    , "ARE, HE EATS YOU UP (& YOU LOSE!)"
    , ""
    , "     YOU:"
    , "EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW"
    , "     MOVING: YOU CAN GO ONE ROOM (THRU ONE TUNNEL)"
    , "     ARROWS: YOU HAVE 5 ARROWS. YOU LOSE WHEN YOU RUN OUT."
    , "     EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING"
-- Fixed typo: "TTHE ROOM#S".
    , "     THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO."
    , "     IF THE ARROW CAN'T GO THAT WAY (IE NO TUNNEL) IT MOVES"
    , "     AT RANDOM TO THE NEXT ROOM."
    , "       IF THE ARROW HITS THE WUMPUS, YOU WIN."
    , "       IF THE ARROW HITS YOU, YOU LOSE."
    , ""
    , "     WARNINGS:"
    , "      WHEN YOU ARE ONE ROOM AWAY FROM WUMPUS OR HAZARD,"
    , "     THE COMPUTER SAYS:"
    , "WUMPUS-    'I SMELL A WUMPUS'"
    , "BAT   -    'BATS NEARBY'"
    , "PIT   -    'I FEEL A DRAFT'"
    , ""
    ]
  output "HUNT THE WUMPUS"
  newGame

newGame = do
  ls <- whilishM ((/=) <*> nub) $
    replicateM 6 $ succ . fromInteger <$> rand 20
  play ls 5 ls

askRooms acc k lim
  | k > lim = pure $ reverse acc
  | otherwise = do
    outputn "ROOM #"
    p <- fromInteger . readInteger <$> input
    if k <= 2 || acc!!1 /= p then askRooms (p:acc) (k + 1) lim else do
-- Fixed typo: "THA TCORRKED".
      output "ARROWS AREN'T THAT CROOKED - TRY ANOTHER ROOM"
      askRooms acc k lim

askWhere h = do
  outputn "WHERE TO"
  loc <- fromInteger . readInteger <$> input
  if loc < 1 || loc > 20 then askWhere h
    else if loc `notElem` dsts h then do
      outputn "NOT POSSIBLE -"
      askWhere h
    else pure loc

moveWumpus m ls@(h:w:rest) cont = do
  r <- fromInteger <$> rand 4
  let
    w' = if r == 3 then w else dsts w!!r
  if w' == h
    then do
      output "TSK TSK TSK - WUMPUS GOT YOU!"
      lose m
    else cont $ h:w':rest

moveHuman m ls@(h:w:_) cont
  | h == w = do
    output "...OOPS! BUMPED A WUMPUS!"
    moveWumpus m ls pitsBats 
  | otherwise = pitsBats ls
  where
  pitsBats ls@[h, w, pit1, pit2, bat1, bat2] = do
    if h == pit1 || h == pit2 then do
      output "YYYIIIIEEEE . . . FELL IN PIT"
      lose m
      else if h == bat1 || h == bat2 then do
        output "ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!"
        h' <- fromInteger <$> rand 20
        moveHuman m (h':tail ls) cont
      else cont ls

lose m = do
  output "HA HA HA - YOU LOSE!"
  askSame m

askSame m = do
  outputn "SAME SET-UP (Y-N)"
  i <- input
  if i == "Y" then play m 5 m else newGame

shootOrMove m ammo ls@(h:w:_) = do
  outputn "SHOOT OR MOVE (S-M)"
  i <- input
  case i of
    "S" -> do
      j9 <- whilishM (`notElem` [1..5]) do
        outputn "NO. OF ROOMS(1-5)"
        readInteger <$> input
      let
        shoot loc = \case
          [] -> do
            output "MISSED"
            moveWumpus m ls $ \ls' -> do
              if ammo > 1
                then play m (ammo - 1) ls'
                else lose m
          p:ps | p `elem` dsts loc -> hit p ps
               | otherwise -> do
                 r <- (dsts loc!!) . fromInteger <$> rand 3
                 hit r ps
        hit p ps
          | p == w = do
            output "AHA! YOU GOT THE WUMPUS!"
            output "HEE HEE HEE - THE WUMPUS'LL GETCHA NEXT TIME!!"
            askSame m
          | p == h = do
            output "OUCH! ARROW GOT YOU!"
            lose m
          | otherwise = shoot p ps
      shoot h =<< askRooms [] 1 j9

    "M" -> do
      h' <- askWhere h
      moveHuman m (h':tail ls) $ play m ammo
    _ -> shootOrMove m ammo ls

play m ammo ls@(h:t) = do
  output ""
  let
    wum = "I SMELL A WUMPUS!"
    dra = "I FEEL A DRAFT!"
    bat = "BATS NEARBY!"
    ds = dsts h
  outputs [m | (x, m) <- zip t [wum, dra, dra, bat, bat], x `elem` ds]
  output $ "YOU ARE IN ROOM  " ++ show h
  output $ "TUNNELS LEAD TO " ++ concatMap (("  " ++) . show) ds
  output ""
  shootOrMove m ammo ls
\end{code}

The 5-room range of an arrow feels overpowered. If you enter a room and smell a
wumpus, then you know it's in one of the two unvisited rooms. These are
necessarily connected by a 4-room path, so typing in the right sequence
guarantees a hit.

It's curious that it's illegal to direct an arrow to return to the room it just
left, but if you give it an impossible path, then there's a chance it will do
just that.

== Bagels ==

https://www.atariarchives.org/basicgames/showpage.php?page=9[The original
Bagels source] converted back and forth between numbers and characters. We
stick to characters, a forgivable deviation since the original program could
have easily done the same.

\begin{code}
bagels = do
  outputs
    [ replicate 32 ' ' ++ "BAGELS"
    , replicate 14 ' ' ++ "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
    , "", "", ""]
  outputn "WOULD YOU LIKE THE RULES (YES OR NO)"
  a <- input
  unless (head a == 'N') $ outputs
    [ "", "I AM THINKING OF A THREE-DIGIT NUMBER.  TRY TO GUESS"
    , "MY NUMBER AND I WILL GIVE YOU CLUES AS FOLLOWS:"
    , "   PICO   - ONE DIGIT CORRECT BUT IN THE WRONG POSITION"
    , "   FERMI  - ONE DIGIT CORRECT AND IN THE RIGHT POSITION"
    , "   BAGELS - NO DIGITS CORRECT"
    ]
  play 0

play y = do
  n <- concatMap show <$>
      whilishM ((/= 3) . length . nub) (replicateM 3 $ rand 10)
  output "O.K.  I HAVE A NUMBER IN MIND."
  let
    guess i
      | i > 20 = do
        outputs ["OH WELL", "THAT'S TWENTY GUESSES.  MY NUMBER WAS " ++ n]
        askAgain y
      | otherwise = do
        let s = "GUESS # " ++ show i
        outputn s
        outputn $ replicate (14 - length s) ' ' 
        a <- input
        \cases
          | length a /= 3 -> do
            output "TRY GUESSING A THREE-DIGIT NUMBER."
            guess i
          | any (`notElem` ['0'..'9']) a -> do
            output "WHAT?"
            guess i
          | length (nub a) /= 3 -> do
            output "OH, I FORGOT TO TELL YOU THAT THE NUMBER I HAVE IN MIND"
            output "HAS NO TWO DIGITS THE SAME."
            guess i
          _ -> do
            let d = length $ filter id $ zipWith (==) a n
            let c = length (filter (`elem` n) a) - d
            if d == 3 then do
                outputs ["YOU GOT IT!!!", ""]
                askAgain $ succ y
              else do
                output $ if c + d == 0 then "BAGELS" else
                    concat $ replicate c "PICO " ++ replicate d "FERMI "
                output ""
                guess $ succ i
  guess 1

askAgain y = do
  outputn "PLAY AGAIN (YES OR NO)"
  a <- input
  if a == "YES" then play y else do
    when (y /= 0) $
        outputs ["", "A " ++ show y ++ " POINT BAGELS BUFF!!"]
    output "HOPE YOU HAD FUN.  BYE."
\end{code}

== Back to the future ==

We hook up the games to this webpage:

\begin{code}
jsEval_ "initDemo(repl);"
runBASIC wumpus
jsEval_ "tty.focus();"
\end{code}

Was Dijkstra right? On the one hand, details like jumps, line numbers, and
global state threaten to overwhelm the programmer, but this is no worse than
assembly. On the other hand, BASIC teaches strict typing via mandatory
https://en.wikipedia.org/wiki/Hungarian_notation[Hungarian notation], and we've
just seen that commands like PRINT and INPUT are well-suited for simple games.

And while I greatly appreciate the convenience of higher-order functions and
recursion in Haskell,
https://modeltheory.org/papers/2018children-recursion.pdf[research suggests
that children as old as 11] struggle to write recursive programs even if they
understand the idea; I speculate the myelination that occurs soon after
this age unlocks the ability. BASIC was age-appropriate for me.

Stepping away from the theory, BASIC is almost always experienced through an
interpreter, rather than a compiler; an interactive Read-Eval-Print-Loop
(REPL), yet one which it was still easy to write big blocks of code due to the
line number convention. The instant feedback encouraged tinkering and
experimentation, and accelerated learning.
