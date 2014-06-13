DATABASE='projekt.db'
PROJECT='CoffeeShop'
CFLAGS=-O3
AUTHOR="ŁukaszCzapliński"

EXAMPLE=examples
DOC=doc
BIN=src

TARGETS=$(BIN) $(DOC) $(EXAMPLE)
CLEAN_TARGETS=$(TARGETS:%=%.clean)
DIST_TARGETS=$(TARGETS:%=%.dist)

export PROJECT
export DATABASE
export CFLAGS

all: $(TARGETS)

clean: $(CLEAN_TARGETS)

$(BIN):
	@$(MAKE) -C $@

$(EXAMPLE):
	@$(MAKE) -C $@

$(DOC):
	@$(MAKE) -C $@

%.clean:
	@$(MAKE) -C $* clean

%.dist:
	@$(MAKE) -C $* dist

dist: $(DIST_TARGETS)
	-rm -rf $(PROJECT)$(AUTHOR).tgz
	tar czf $(PROJECT)$(AUTHOR).tgz ./*

.PHONY: all dist clean $(TARGETS)
