.PHONY: sync site target
target: site

redcode.js: redcode.lhs ; hastec --opt-all $^
redcode.html: ../play/menu redcode.lhs ; cobble mathbook $^

site: redcode.js redcode.html ../haskell/menu.html

sync: site
	rsync redcode.html redcode.js redcode.lhs xenon.stanford.edu:www/play/
