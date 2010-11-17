# For maintainers. Use `cabal install` instead.

CTAGS = hasktags --ctags
ETAGS = hasktags --etags

SRC = $(shell find Network -type f -name '*.hs')

.PHONY: all maintainer-clean

all:

maintainer-clean:
	@rm -f TAGS tags ChangeLog

ChangeLog:
	@git log --summary HEAD > ChangeLog

TAGS: $(SRC)
	@$(ETAGS) $^

tags: $(SRC)
	@$(CTAGS) $^
