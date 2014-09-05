CASK=cask
EMACS=emacs

all: gensource test build

viewing: gensource build test

really-all: install all

install:
	$(CASK) install

## at the moment, this breaks where
gensource:
	$(CASK) exec emacs --debug --script script/gensource.el -- generate

test:
	lein test

pdf:
	a2x --dblatex-opts="--debug" --dblatex-opts="--texinputs=./tex//" \
	--dblatex-opts="--texstyle=take-wing" 	book.asciidoc

html:
	asciidoc book.asciidoc

build: html pdf

.PHONY: test
