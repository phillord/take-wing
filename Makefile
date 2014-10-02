CASK=cask
EMACS=emacs
WING=$(CASK) exec emacs --debug --script script/build.el --

all: gen-src test publish

viewing: gen-src test tex

really-all: install all


install:
	$(CASK) install

## at the moment, this breaks where
gen-src:
	$(WING) gen-src

test: gen-src
	lein test

publish:
	$(WING) publish
	cp tex/clojure.sty tex/tawny.sty exports
	# org will publish to PDF but puts it in the wrong place
	cd exports;pdflatex take_wing.tex;pdflatex take_wing.tex

clean:
	- rm exports/*
	- rm src/take/wing/*clj

-include Makefile-local

.PHONY: test build tex
