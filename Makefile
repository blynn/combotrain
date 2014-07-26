all : 15 js.js
15 : 15.hs ; ghc $^
js.js : js.hs ; hastec $^
