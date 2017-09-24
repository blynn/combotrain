= Let's Play! =

http://benlynn.blogspot.com/2014/07/15-shades-of-grey_29.html[Inspired by a
talk by John Carmack]
[http://benlynn.blogspot.com/2014/08/let-code.html[follow-up post]], I wrote
these browser games in Haskell. I used the http://haste-lang.org/[Haste
compiler] to solve https://wiki.haskell.org/The_JavaScript_Problem[the
JavaScript problem].

I chose Haste because https://github.com/ghcjs/ghcjs[GHCJS] seemed tough to
install at the time.

I could mostly pretend it was Haskell as usual, though some differences
arose.

== JavaScript FFI ==

Calling Haskell functions from JavaScript and vice versa is painless.
We'll demonstrate calling Haskell from JavaScript and vice versa:

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<style type="text/css">
.term {
  font-family: 'Inconsolata', monospace;
  font-size: 90%;
  color: #00aa00;
  background: #000000;
}
.term .termReverse {
  background: #00aa00;
  color: #000000;
}
table { margin: 0; }
</style>
<script type="text/javascript" src="/~blynn/termlib.js"></script>
<script type="text/javascript">
var term = new Terminal( {handler: termHandler, greeting:
  'Try typing: foo "quotes!!11!" 42 3.1415 and 9e3',
  cols: 48, rows: 16} );
function termHandler() {
  this.newLine();
  var line = Haste.wow(this.lineBuffer);
  if (line != "") this.write(line);
  this.prompt();
}
function termOpen() { term.open(); }
</script>
<div id="termDiv"></div>
<script type="text/javascript" src="index.js"></script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Let's walk through the details.

Strings are a constant source of friction, as JavaScript and Haskell have
represent them differently. (In fact, Haskell has many representations, and
its most well-known one, `String`, is often terrible.)

Enabling the `OverloadedStrings` extension marginally reduces the number
of conversions between `String` and `JSString`.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Maybe
import Haste.Foreign
import Haste.Prim
\end{code}

Our demo features
https://www.reddit.com/r/haskell/comments/3r75hq/blow_my_mind_in_one_line/[a mind-blowing
Haskell one-liner], the function `readMany`, which extracts readable
Haskell values of a given type. The `wow` functions calls it to extract values
of type `Int`, `Double` and `String` from a given `JSString`, and shows the
results in a `JSString`.

We call `ffi` to grant our code access to the JavaScript `termOpen` function,
which opens the terminal. The type declaration here is mandatory.

The `export` function performs the converse, that is, grants JavaScript access
to our `wow` function.

\begin{code}
readMany :: Read a => String -> [a]
readMany = unfoldr $ listToMaybe . concatMap reads . tails

wow :: JSString -> JSString
wow js = let s = fromJSStr js in toJSStr $ unlines $ [
  "Ints:    " ++ show (readMany s :: [Int]),
  "Doubles: " ++ show (readMany s :: [Double]),
  "Strings: " ++ show (readMany s :: [String])]

main :: IO ()
main = do
  export "wow" wow
  ffi "termOpen" :: IO ()
\end{code}

The above ties in with the JavaScript below. The `termlib.js` refers to the
excellent http://www.masswerk.at/termlib/[termlib] JavaScript library.

[source,html]
------------------------------------------------------------------------------
<style type="text/css">
.term {
  font-family: 'Inconsolata', monospace;
  font-size: 90%;
  color: #00aa00;
  background: #000000;
}
.term .termReverse {
  background: #00aa00;
  color: #000000;
}
table { margin: 0; }
</style>
<script type="text/javascript" src="/~blynn/termlib.js"></script>
<script type="text/javascript">
var term = new Terminal( {handler: termHandler, greeting:
  'Try typing: foo "quotes!!11!" 42 3.1415 and 9e3',
  cols: 48, rows: 16} );
function termHandler() {
  this.newLine();
  var line = Haste.wow(this.lineBuffer);
  if (line != "") this.write(line);
  this.prompt();
}
function termOpen() { term.open(); }
</script>
<div id="termDiv"></div>
<script type="text/javascript" src="index.js"></script>
------------------------------------------------------------------------------

== Threads ==

https://developer.mozilla.org/en/docs/Web/JavaScript/EventLoop[JavaScript is
single-threaded], in the sense that a function is run to completion before
another begins. There is a implicit event loop running the show.

Redraws only occur when control is returned to the event loop, which can
require sending timeout events.

When programming with libraries such as SDL, we manage multiple threads,
and redraws are under our control. This might make it challenging for a
browser game and a Linux game to share code, though I've only explored this
briefly. Perhaps writing an event loop to mimic JavaScript is the easiest
solution.

An alternative may be `Haste.Concurrent`, which appears to simulate multiple
threads in JavaScript.

== HTML Canvas ==

Experience with SDL helps somewhat with drawing to a canvas element. For
example, `createRGBSurface` is analagous to `createCanvas`.

On the other hand, instead of pixels with integer coordinates, the canvas
uses points with floating point coordinates.

== UI woes ==

Haste has limited support for HTML events. For an
link:../haskell/enigmalhtml[Enigma machine simulation],
I wanted to intercept
https://developer.mozilla.org/en-US/docs/Web/Events/input[the input event].
The Haste API lacks this event.

I worked around the problem by writing a snippet of JavaScript to fire off a
scroll event on an input event, which Haste does recognize:

------------------------------------------------------------------------------
<script type="text/javascript">
var ev = new Event('scroll');
function genEv() {
  document.getElementById("grundstellung").dispatchEvent(ev);
}
</script>
<textarea oninput="genEv();"></textarea>
------------------------------------------------------------------------------

== Big data ==

I gave up trying to embed a neural network and some test cases in
link:../haskell/brain.html[a handwritten digit recogniition demo].

I tried defining a giant list in Haskell. I tried a routine to fetch and read
a text file. In the end I was forced to write some JavaScript.

== haste-cabal ==

For a long time, I thought I was confined to the packages bundled with Haste.

On the contrary, by running `haste-cabal`, we can use many packages in our
JavaScript programs:

------------------------------------------------------------------------------
$ haste-cabal update
$ haste-cabal install random
$ haste-cabal install parsec
------------------------------------------------------------------------------
