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

PDFLATEX=pdflatex -interaction=nonstopmode

pdf:
	cd latex;$(PDFLATEX) take_wing.tex;$(PDFLATEX) take_wing.tex

html:
	cd latex;htlatex "take_wing" "cf,fn-in" "" "" "-interaction=nonstopmode"

publish: pdf html

clean:
	# There really has to be a better way than this!
	- find latex -not -name "*tex" -not -name ".gitignore" \
		-not -name "cf.cfg" -not -name "*css" -not -name "*js" \
		-not -name "Makefile" -not -name ".dir-locals.el" \
		-not -name "*org" -not -name "*sty" \
		-print -exec rm {} \;
	- rm latex/_region_*
	- rm latex/_buffer_*
	- rm src/take/wing/*clj

-include Makefile-local

.PHONY: test build tex
