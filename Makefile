.PHONY: all sync push target
target: all

HSFILES=tictactoe netwalk breakthrough peg chess index redcode 15

SITEFILES=$(addsuffix .html, $(HSFILES)) $(addsuffix .js, $(HSFILES)) xo.png

HS2JS=-mv Main.jsmod /tmp; hastec -Wall --opt-all

menu.html: menu ; cobble menu menu
%.html : %.lhs menu.html ; cobble mathbook menu $<

all: $(SITEFILES)
%.js : %.lhs ; $(HS2JS) $^

git-push: ; git push git@github.com:blynn/combotrain.git master
sync: all; rsync $(SITEFILES) crypto.stanford.edu:www/play/

15 : 15-sdl.hs ; ghc $^

asterius/15.wasm asterius/15.js: 15.lhs
	mkdir -p asterius
	cp 15.lhs asterius/
	podman run -it --rm -v `pwd`/asterius:/mirror -w /mirror terrorjack/asterius:latest ahc-link --bundle --browser --ghc-option -O --input-hs 15.lhs
