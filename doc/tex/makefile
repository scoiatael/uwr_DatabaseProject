CC=xelatex
ABOUT=about
GUIDE=guide
TECH=tech

TARGETS=$(ABOUT) $(GUIDE) $(TECH)
BIN=$(TARGETS:%=%.pdf)

all:$(TARGETS)

$(ABOUT) : $(ABOUT).pdf
$(GUIDE) : $(GUIDE).pdf
$(TECH) : $(TECH).pdf

%.pdf : %.tex
	@echo "Making $*..."
	@$(CC) $*.tex > /dev/null
	@echo "..recreating indexes.."
	@$(CC) $*.tex > /dev/null
	@echo "Done"

clean:
	-rm -rf *.toc *.log *.aux *.out $(BIN)

dist: all
	-@mkdir bin
	-mv $(BIN) bin/

.PHONY: clean all $(TARGETS) dist
