# For maintainers. Use `cabal install` instead.

CTAGS = hasktags --ctags
ETAGS = hasktags --etags

SRC = $(shell find Network -type f -name '*.hs')

.PHONY: all maintainer-clean

all:

install-hooks:
	cd $(shell git rev-parse --show-toplevel)
	cp hooks/pre-commit .git/hooks/pre-commit
	chmod +x .git/hooks/pre-commit

maintainer-clean:
	@rm -f TAGS tags ChangeLog

ChangeLog:
	@git log --summary HEAD > ChangeLog

TAGS: $(SRC)
	@$(ETAGS) $^

tags: $(SRC)
	@$(CTAGS) $^
