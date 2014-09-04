CASK=cask
EMACS=emacs

all: install gensource test build

install:
	$(CASK) install

## at the moment, this breaks where
gensource:
	$(CASK) exec emacs --debug --script script/gensource.el -- generate

test:
	lein test

build:
	asciidoc book.asciidoc

.PHONY: test
