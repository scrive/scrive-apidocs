DBNAME=kontratest
DBUSER=kontra
DBPASSWORD=kontra
TESTS=all


.PHONY : all
all:
	cabal install -f-test

# Make "pretty" diagram of database model (requires postgresql-autodoc and graphwiz)
dist/dbmodel/$(DBNAME).png:
	mkdir -p dist/dbmodel
	cd dist/dbmodel && postgresql_autodoc -t dot -d $(DBNAME) -u $(DBUSER) --password=$(DBPASSWORD)
	dot -Tpng dist/dbmodel/$(DBNAME).dot -o dist/dbmodel/$(DBNAME).png

# Build and run all tests
.PHONY : test
test:
	cabal install -f-server -ftest-coverage
	rm -f kontrakcja-test.tix
	time dist/build/kontrakcja-test/kontrakcja-test $(TESTS)

# Create coverage pages from test run
.PHONY : hpc
hpc:
	build-scripts/createCoverageReports.sh
	@echo "Now open coverage-reports/hpc_index.html"

# Restore the test database to its original condition
.PHONY : reset-test-db
reset-test-db:
	-PGUSER=$(DBUSER) dropdb $(DBNAME)
	PGUSER=$(DBUSER) createdb $(DBNAME)
	psql $(DBNAME) $(DBUSER) -c "ALTER DATABASE $(DBNAME) SET TIMEZONE = 'UTC';"

