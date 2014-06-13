DUMPFILE='dump.sql'

all: populate_database

populate_database:
	@echo "Populating database.."
	psql $(DATABASE) < $(DUMPFILE)
	@echo "Done."

clean:

dist:

.PHONY: populate_database all clean
