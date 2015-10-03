.PHONY: all push
all: tictactoe.js netwalk.js breakthrough.js chess.js
%.js : %.hs ; hastec --opt-whole-program $^
git-push:
	git push https://code.google.com/p/combotrain master
	git push git@github.com:blynn/combotrain.git master
