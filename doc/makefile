HADDOCK=html
TEX=tex

TARGETS=$(HADDOCK) $(TEX)
CLEAN_TARGETS=$(TARGETS:%=%.clean)

all: $(TARGETS)

$(TEX):
	@$(MAKE) -C $@

$(HADDOCK):
	@$(MAKE) -C $@

%.clean:
	@$(MAKE) -C $* clean

clean: $(CLEAN_TARGETS)

.PHONY: all clean $(TARGETS)
