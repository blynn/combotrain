= Let's Play! =

http://benlynn.blogspot.com/2014/07/15-shades-of-grey_29.html[Inspired by a
talk by John Carmack]
[http://benlynn.blogspot.com/2014/08/let-code.html[follow-up post]], I wrote
some browser games in Haskell.

To solve https://wiki.haskell.org/The_JavaScript_Problem[the JavaScript
problem], I initially used the http://haste-lang.org/[Haste compiler]. The
https://github.com/ghcjs/ghcjs[GHCJS] compiler seemed heavyweight and tough to
install. It worked great. I could mostly pretend it was Haskell as usual.

Sadly, Haste appears to be abandoned. The good news is that GHC is gaining
JavaScript and WebAssembly backends. I would migrate, but my goals have
shifted. Simple games ought to be simple to write. For toy programs, there
should be no need for setting up complex development environments and lengthy
compile times and lengthy boilerplate.

== Plug and play ==

When a game is sufficiently short and sweet, I use a version of
link:../compiler/[my own Haskell compiler] because of some features:

  * Zero-install. The webpage loads a wasm binary version of my compiler and
  runs it on the code within.

  * Helpers for global variables. Our `global` and `setGlobal` functions are
  slightly less unprincipled than `unsafePerformIO` with `newIORef`. This suits
  the event-driven nature of the DOM, as we can spread code among many
  top-level functions rather than stuff everything into a single main function.

  * Interactive REPL. Anyone can edit and run the code on the page (though of
  course changes cannot be saved).

  * Module fetching. Even halfway through a program we can fetch object files
  elsewhere on my website and import their definitions. I no longer need to
  start every program with a series of imports.

  * In the same vein, my compiler uses my preferred language options so I no *
  longer have to declare them at the start.

Our code hits the ground running. For example, the following computes the 100th
Fibonacci number. You can edit it and run it again: click the button or press
Ctrl-Enter. Pressing Alt-Enter will run it and give you a new box to add more
code, with access to all previous definitions.

\begin{code}
fibs=0:1:zipWith (+) fibs (tail fibs)
fibs !! 100
\end{code}

== Back to BASICs ==

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<link href="https://fonts.googleapis.com/css2?family=Fira+Code:wght@300..700&family=Sixtyfour&display=swap" rel="stylesheet">
<style type="text/css">
.term {
  font-family: 'Sixtyfour', monospace;
  color: #00ff00;
  background: #000000;
}
.term .termReverse {
  background: #00ff00;
  color: #000000;
}
table { margin: 0; }
</style>
<script src="/~blynn/termlib.js"></script>

<button id="spiderwoman">REDO FROM START</button>
<div id="spiderwomanDiv"></div>
<script>
let run;
let spiderterm;
function spiderblur() {
  spiderterm.cursorOff();
  spiderterm.lock = true;
}
function initSpiderwoman(repl) {
  spiderterm = new Terminal({
    handler: termHandler, greeting: '',
    termDiv: 'spiderwomanDiv', ps: '?', cols: 40, rows: 25,
    closeOnESC: false,
    ctrlHandler: function() { if (this.inputChar == termKey.ESC) TermGlobals.keylock = true; }
  });
  run = (s) => repl.run("chat", ["Main"], s);
  function termHandler() {
    this.newLine();
    run("inputCont [r|" + this.lineBuffer + "|]");
  }
  spiderterm.open();
  function spiderfocus(ev) {
    TermGlobals.activeTerm = spiderterm;
    spiderterm.lock = false;
    spiderterm.cursorOn();
    ev.stopPropagation();
    let oneshot;
    oneshot = document.body.addEventListener("click", (ev) => {
      spiderblur(); document.body.removeEventListener("click", oneshot);
    });
  }
  spiderwomanDiv.addEventListener("click", spiderfocus);
}
</script>
<br>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

In my offline childhood, source code was hard to come by, so I was always
elated if I saw https://usborne.com/us/books/computer-and-coding-books[computer
programming books published by Usborne in the 1980s] at my local library.

Let's attempt to remake these games. We use
http://www.masswerk.at/termlib/[termlib] and a funny font to simulate an old
computer. We define versions of common BASIC commands. Since `print` is a
Haskell keyword, we use `output` instead of `PRINT`, and also define `outputs`,
which prints a list of strings.

There is an awkward mismatch with the `INPUT` command due to the asynchronous
nature of the DOM. We work around this by defining `input` to take a
continuation as an argument. Our `delay` function is similar. In both cases,
we abuse the `global` and `setGlobal` hacks to store continuations. This means
there is a race condition, though it's difficult to trigger.

\begin{code}
rand n = fromInteger . readInteger <$> jsEval ("Math.floor(Math.random() * " ++ show n ++ ");")

cls = jsEval_ "spiderterm.clear();"
output s = jsEval_ $ "spiderterm.write(`" ++ s ++ "\n`);"
outputs = mapM_ output

input cont = do
  jsEval "spiderterm.prompt();"
  setGlobal cont

inputCont s = global >>= ($ toUpper <$> s)

delay t cont = do
  setGlobal cont
  jsEval_ $ "setTimeout(() => run('delayCont'), " ++ show t ++ ");"

delayCont = global >>= id

toUpper c
  | 'a' <= c && c <= 'z' = chr $ n - 32
  | otherwise = c
  where n = ord c
\end{code}

Now for the fun part: we convert the BASIC of yesteryear to Haskell.
Let's take Spiderwoman from
https://drive.google.com/file/d/0B2Z4GOoRXHWUVzBtTmxpV09NWFk/view?resourcekey=0-MTEoOx2EfESeq0ZBKvx25Q[Creepy
Computer Games], where Spiderwoman is thinking of a letter that the player must
guess by typing in words.

\begin{code}
lose = output "YOU ARE NOW A FLY"

play g choice = do
  outputs ["", "TRY A WORD", "", ""]
  input check
  where
  next = play (g + 1) choice
  check w
    | length w < 4 || length w > 8 = next
    | choice `elem` w = do
      outputs ["YES - IT'S ONE OF THOSE", "", "DO YOU WANT TO GUESS ? (Y OR N)"]
      input \r -> if r == "N" then cls *> next else do
        outputs ["", "WHAT IS YOUR GUESS THEN ? "]
        input \g -> if g /= [choice]
          then lose
          else outputs ["OK - YOU CAN GO", "(THIS TIME)"]
    | otherwise = do
      outputs ["", "IT'S NOT IN THAT WORD"]
      delay 500 $ cls *> if g > 15
        then output "YOU ARE TOO LATE" *> lose
        else next

spiderwoman = do
  cls
  outputs ["SPIDERWOMAN", "HAS CHOSEN"]
  play 1 =<< (['A'..'Z']!!) <$> rand 26
\end{code}

Lastly, we write some glue code to connect HTML elements to our code:

\begin{code}
jsEval_ "initSpiderwoman(repl);"
jsEval_ "spiderwoman.addEventListener('click', ev => run('spiderwoman'));"
spiderwoman
jsEval_ "spiderblur();"
\end{code}
