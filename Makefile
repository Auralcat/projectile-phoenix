CASK = cask
export EMACS ?= emacs

elpa-$(EMACS):
	$(CASK) install
	$(CASK) update
	touch $@

elpa: elpa-$(EMACS)

test: elpa
	$(CASK) exec buttercup -L .
