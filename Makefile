CASK=cask
EMACS=emacs
WING=$(CASK) exec emacs --debug --script script/build.el --

all: gen-src test build chunked

viewing: gen-src build test

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

pdf:
	$(WING) pdf

html:
	$(WING) html

build: html pdf

clean:
	- rm exports/*
	- rm src/take/wing/*clj

.PHONY: test build
