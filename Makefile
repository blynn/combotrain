.PHONY: all
all: tictactoe.js netwalk.js
%.js : %.hs ; hastec $^
