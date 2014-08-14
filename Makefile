.PHONY: all push
all: tictactoe.js netwalk.js breakthrough.js
%.js : %.hs ; hastec $^
push:
	git push https://code.google.com/p/combotrain master
	git push git@github.com:blynn/combotrain.git master
