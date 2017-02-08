.PHONY: all sync push target
target: all

HSFILES=tictactoe netwalk breakthrough peg chess

SITEFILES=$(addsuffix .html, $(HSFILES)) $(addsuffix .js, $(HSFILES)) \
  index.html xo.png

HS2JS=-mv Main.jsmod /tmp; hastec --opt-all

menu.html: menu ; cobble menu menu
%.html : %.txt menu.html ; cobble mathbook menu $<
%.html : %.lhs menu.html ; cobble mathbook menu $<

all: $(SITEFILES)
%.js : %.lhs ; $(HS2JS) $^

# See comments in chess.hs.
chess.js : oldchess.js ; cp $^ $@

git-push: ; git push git@github.com:blynn/combotrain.git master
sync: all; rsync $(SITEFILES) xenon.stanford.edu:www/play/
