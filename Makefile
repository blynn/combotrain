.PHONY: all push
all: tictactoe.js netwalk.js breakthrough.js chess.js peg.js
%.js : %.hs ; hastec --opt-whole-program $^
git-push: ; git push git@github.com:blynn/combotrain.git master
