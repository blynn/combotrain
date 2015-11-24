all : 15 js.js
15 : 15.hs ; ghc $^
js.js : js.hs ; hastec $^
sync: js.js; scp $^ blynn@xenon.stanford.edu:www/15/
