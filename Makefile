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

15 : 15.hs ; ghc $^
