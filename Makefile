DBNAME=kontratest
DBUSER=kontra
TESTS=all --plain

INSTALL=BUILD_DATE=2010-01-01-00-00-00 cabal-dev install $(INSTALLFLAGS)

.PHONY : all
all:
	$(INSTALL) -f-test

# Make "pretty" diagram of database model (requires postgresql-autodoc and graphwiz)
.PHONY: dot
dot:
	mkdir -p dist/dbmodel
	cd dist/dbmodel && postgresql_autodoc -t dot -l ../../postgresql_autodoc -d $(DBNAME) -u $(DBUSER)
	dot -Tpng dist/dbmodel/$(DBNAME).dot -o dist/dbmodel/$(DBNAME).png
	dot -Tsvg dist/dbmodel/$(DBNAME).dot -o dist/dbmodel/$(DBNAME).svg

JDBCDRIVER=/usr/share/java/postgresql-jdbc3.jar
SCHEMASPY=schemaSpy_5.0.0.jar
# Make browsable schema documentation in dist/schema/index.html using SchemaSpy
.PHONY: schemaspy
schemaspy:
	java -jar $(SCHEMASPY) -t pgsql -host localhost -db $(DBNAME) -u $(DBUSER) -o dist/schema -dp $(JDBCDRIVER) -pfp -s public

# Build and run all tests
.PHONY : test
test:
	$(INSTALL) -f-server -f-cron -f-mailing-server -ftest-coverage
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

# Heap profiling

# Consider removing all user-installed hackage libraries first, so
# that all libraries are compiled with the additional profiling flags
# to get detailed profiling

.PHONY : profiling
profiling:
	$(INSTALL) --only-dependencies --enable-library-profiling --ghc-options="-auto-all -caf-all"
	cabal-dev configure --enable-executable-profiling --ghc-options="-auto-all -caf-all"
	cabal-dev build
