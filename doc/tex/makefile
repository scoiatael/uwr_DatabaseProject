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

%.pdf : %.tex main.tex images/*
	@echo "Making $*..."
	@$(CC) $*.tex
	@echo "..recreating indexes.."
	$(CC) $*.tex > /dev/null
	@echo "Done"

clean:
	-rm -rf *.toc *.log *.aux *.out $(BIN)

dist: all 
	-@mkdir -p bin
	-mv $(BIN) bin/

.PHONY: clean all $(TARGETS) dist
