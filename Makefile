CASK=cask
EMACS=emacs
WING=$(CASK) exec emacs --debug --script script/build.el --

all: gensource test build chunked

viewing: gensource build test

really-all: install all

install:
	$(CASK) install

## at the moment, this breaks where
gensource:
	$(CASK) exec emacs --debug --script script/gensource.el -- generate

test: gensource
	lein test

pdf:
	a2x --dblatex-opts="--debug" --dblatex-opts="--texinputs=./tex//" \
	--dblatex-opts="--texstyle=take-wing" book.asciidoc
	cp book.pdf ~/Dropbox/temp/


chunked:
	a2x --format chunked book.asciidoc

html:
	asciidoc book.asciidoc

build: html pdf

clean:
	rm book.pdf
	rm book.html
	rm src/take/wing/*clj

.PHONY: test build
