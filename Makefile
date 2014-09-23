CASK=cask
EMACS=emacs
WING=$(CASK) exec emacs --debug --script script/build.el --

all: gen-src test publish tex

viewing: gen-src build test

really-all: install all

tex:
	echo "Running tex"
	$(CP) tex/clojure.sty tex/tawny.sty exports
	cd exports;latex take_wing.tex

install:
	$(CASK) install

## at the moment, this breaks where
gen-src:
	$(WING) gen-src

test: gen-src
	lein test

publish:
	$(CP) tex/clojure.sty tex/tawny.sty exports
	$(WING) publish

publish-live:
	$(WING) publish-live

clean:
	- rm exports/*
	- rm src/take/wing/*clj

.PHONY: test build tex
