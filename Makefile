CASK = cask
EMACS = emacs
EMACSFLAGS = --batch --quick --directory=. --file=ruby-test-mode.el

all: package

.PHONY: clean test

.cask:
	$(CASK) install

checkdoc:
	$(CASK) exec $(EMACS) $(EMACSFLAGS) --eval="(checkdoc)"

clean:
	@rm -rf dist
	$(CASK) clean-elc

compile: .cask
	$(CASK) exec $(EMACS) $(EMACSFLAGS) --eval="(batch-byte-compile)"

distclean: clean
	@rm -rf .cask

lint: .cask
	$(CASK) exec $(EMACS) $(EMACSFLAGS) --eval="(elint-directory \".\")"

package: test checkdoc
	$(CASK) package

test: .cask
	$(CASK) exec ert-runner
