CASK = cask
EMACS = emacs
EMACSFLAGS = --batch -L . 

all: package

.PHONY: clean test

.cask:
	$(CASK) install

checkdoc:
	$(CASK) exec $(EMACS) $(EMACSFLAGS) --eval="(checkdoc)" -Q ruby-test-mode.el

clean:
	@rm -rf dist
	$(CASK) clean-elc

compile: .cask
	$(CASK) exec $(EMACS) $(EMACSFLAGS) --eval="(batch-byte-compile)" -Q ruby-test-mode.el

distclean: clean
	@rm -rf .cask

lint: .cask
	$(CASK) exec $(EMACS) $(EMACSFLAGS) --eval="(elint-directory \".\")" 

package: test checkdoc
	$(CASK) package

test: .cask
	$(CASK) exec ert-runner
