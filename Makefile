all : 15 15.js 15.html
15 : 15.hs ; ghc $^
15.js : 15.lhs ; hastec -Wall $^
15.html : ../play/menu 15.lhs ; cobble mathbook $^
sync: 15.lhs 15.js 15.html; scp $^ crypto.stanford.edu:www/play/
