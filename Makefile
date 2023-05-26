.PHONY: all sync push target
target: all

SHELL=/bin/bash -o pipefail

HSFILES=tictactoe breakthrough peg chess index redcode 15

SITEFILES=netwalk.html netwalk.wasm $(addsuffix .html, $(HSFILES)) $(addsuffix .js, $(HSFILES)) xo.png

HS2JS=-mv Main.jsmod /tmp; hastec -Wall --opt-all

netwalk.html:netwalk.lhs menu.html;stitch book menu $<
COMPILER=compiler
netwalk.c:netwalk.lhs;(cat $(COMPILER)/inn/BasePrecisely.hs $(COMPILER)/inn/SystemWasm.hs $(COMPILER)/inn/Map1.hs ; $(COMPILER)/unlit < $<) | $(COMPILER)/precisely wasm > $@
%.o: %.c; clang --target=wasm32 -O2 -ffreestanding -c $^ -o $@
%.wasm: %.o; wasm-ld --import-undefined --no-entry --initial-memory=838860800 $^ -o $@

menu.html: menu ; stitch menu $<
%.html : %.lhs menu.html ; stitch book menu $<

all: $(SITEFILES)
%.js : %.lhs ; $(HS2JS) $^

git-push: ; git push git@github.com:blynn/combotrain.git master
sync: all; rsync $(SITEFILES) crypto.stanford.edu:www/play/

15 : 15-sdl.hs ; ghc $^

asterius/15.wasm asterius/15.js: 15.lhs
	mkdir -p asterius
	cp 15.lhs asterius/
	podman run -it --rm -v `pwd`/asterius:/mirror -w /mirror terrorjack/asterius:latest ahc-link --bundle --browser --ghc-option -O --input-hs 15.lhs
