CASK=cask
EMACS=emacs
WING=$(CASK) exec emacs --debug --script script/build.el --

all: gen-src test publish

viewing: gen-src test tex

really-all: install all

# Do this on travis, so that we can see the errors from latex
# if there are any. In practice, will have to switch the pdf publication
# in wing-config.el or we won't get here
travis: publish
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
