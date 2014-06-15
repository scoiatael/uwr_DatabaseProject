PROJECT?='CoffeeShop'

all: create_binary

create_binary:
	ghc -o $(PROJECT) $(CFLAGS) Main.hs

clean:
	-rm -rf *.o *.hi $(PROJECT)

run:
	-@runghc Main

dist: create_binary
	-@mkdir -p bin
	mv $(PROJECT) bin/

.PHONY: clean run create_binary dist all
