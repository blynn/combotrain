.PHONY: all sync push target
target: all

SHELL=/bin/bash -o pipefail

HSFILES=tictactoe breakthrough peg chess index redcode 15

RUNMES=netwalk
$(foreach x,$(RUNMES),$(x).html):%.html:%.run
RUNFILES=$(addsuffix .lhs, $(RUNMES)) $(addsuffix .html, $(RUNMES))

SITEFILES=$(RUNFILES) $(addsuffix .html, $(HSFILES)) $(addsuffix .js, $(HSFILES)) xo.png

HS2JS=-mv Main.jsmod /tmp; hastec -Wall --opt-all

menu.html: menu ; stitch menu $<
%.html: %.run menu.html ; stitch book menu $<
%.html: %.lhs menu.html ; stitch book menu $<
%.run: %.lhs;(sed 's/\\begin{code}/[.runme]\n--------/;s/\\end{code}/--------/' $< ;\
echo '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++';\
echo '<link href="../compiler/runme.css" rel="stylesheet" type="text/css">';\
echo '<script src="../compiler/reply.js" defer></script>';\
echo '<script src="../compiler/runme.js" defer></script>';\
echo '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++') > $@

all: $(SITEFILES)
%.js : %.lhs ; $(HS2JS) $^

git-push: ; git push git@github.com:blynn/combotrain.git master
sync: all; rsync $(SITEFILES) crypto.stanford.edu:www/play/

15 : 15-sdl.hs ; ghc $^

asterius/15.wasm asterius/15.js: 15.lhs
	mkdir -p asterius
	cp 15.lhs asterius/
	podman run -it --rm -v `pwd`/asterius:/mirror -w /mirror terrorjack/asterius:latest ahc-link --bundle --browser --ghc-option -O --input-hs 15.lhs
