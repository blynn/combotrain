.PHONY: sync site target
target: site

redcode.js: redcode.lhs ; hastec --opt-all $^
redcode.html: redcode.lhs ; cobble mathbook ../haskell/menu $^

site: redcode.js redcode.html

sync: site
	rsync redcode.{html,js,lhs} blynn@xenon.stanford.edu:www/haskell/
