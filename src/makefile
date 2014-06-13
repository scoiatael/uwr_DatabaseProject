PROJECT?='CoffeeShop'

all: create_binary

create_binary:
	ghc -o $(PROJECT) $(CFLAGS) Frontend.hs

clean:
	-rm -rf *.o *.hi $(PROJECT)

run:
	runghc Frontend

dist: create_binary
	-@mkdir -p bin
	mv $(PROJECT) bin/

.PHONY: clean run create_binary dist all
