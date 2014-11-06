DBNAME=kontratest
DBUSER=kontra
DBBACKUP=$(DBNAME).dump
TESTS= --plain

CONFIGURE_FLAGS=--disable-optimization

CABAL_CONFIGURE=cabal configure $(CONFIGURE_FLAGS)

.PHONY : all
all:
	cabal sandbox init
	cabal install --only-dependencies -j
	$(CABAL_CONFIGURE)
	cabal build

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
test: all
	rm -f kontrakcja-test.tix
	time dist/build/kontrakcja-test/kontrakcja-test $(TESTS)


# Create evidence texts in evidencetexts
.PHONY: evidencetexts
evidencetexts:
	$(MAKE) test TESTS="--output-dir dist/evidence-texts  --plain"

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

# Make a backup of the test database
backup-test-db:
	pg_dump -U $(DBUSER) $(DBNAME) -F c -f $(DBBACKUP)

# Reset and restore the test database
restore-test-db: reset-test-db
	pg_restore -U $(DBUSER) -n public -d $(DBNAME) $(DBBACKUP)

# Heap profiling

# Consider removing all user-installed hackage libraries first, so
# that all libraries are compiled with the additional profiling flags
# to get detailed profiling

.PHONY : profiling
profiling:
	cabal sandbox init
	cabal install --only-dependencies --enable-library-profiling --ghc-options="-auto-all -caf-all"
	$(CABAL_CONFIGURE) --enable-executable-profiling --ghc-options="-auto-all -caf-all"
	cabal build
