PROJECT?='CoffeeShop'

create_binary:
	ghc -o $(PROJECT) $(CFLAGS) Frontend.hs

clean:
	-rm -rf *.o *.hi $(PROJECT)

run:
	runghc Frontend

.PHONY: clean run create_binary
