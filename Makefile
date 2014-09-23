CASK=cask
EMACS=emacs
WING=$(CASK) exec emacs --debug --script script/build.el --

all: gen-src test publish

viewing: gen-src test tex

really-all: install all

travis:
	## Do this on travis, so that we can see the errors from latex
	## if there are any.
	mkdir -p exports
	cp --update tex/clojure.sty tex/tawny.sty exports
	cd exports;pdflatex take_wing.tex

install:
	$(CASK) install

## at the moment, this breaks where
gen-src:
	$(WING) gen-src

test: gen-src
	lein test

publish:
	mkdir -p exports
	cp tex/clojure.sty tex/tawny.sty exports
	$(WING) publish

clean:
	- rm exports/*
	- rm src/take/wing/*clj

.PHONY: test build tex
