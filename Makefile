.PHONY: all sync push target
target: all

SHELL=/bin/bash -o pipefail

HSFILES=breakthrough chess redcode

RUNMES=netwalk 15 peg index tictactoe
$(foreach x,$(RUNMES),$(x).html):%.html:%.run
RUNFILES=$(addsuffix .lhs, $(RUNMES)) $(addsuffix .html, $(RUNMES))

SITEFILES=$(RUNFILES) $(addsuffix .html, $(HSFILES)) $(addsuffix .js, $(HSFILES))

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
