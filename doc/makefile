HADDOCK=html
TEX=tex

TARGETS=$(HADDOCK) $(TEX)
CLEAN_TARGETS=$(TARGETS:%=%.clean)
DIST_TARGETS=$(TARGETS:%=%.dist)

all: $(TARGETS)

$(TEX):
	@$(MAKE) -C $@

$(HADDOCK):
	@$(MAKE) -C $@

%.clean:
	@$(MAKE) -C $* clean

%.dist:
	@$(MAKE) -C $* dist

clean: $(CLEAN_TARGETS)

dist: $(DIST_TARGETS)

.PHONY: all clean $(TARGETS) dist
