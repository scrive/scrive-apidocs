DBNAME=kontratest
DBUSER=kontra
DBPASSWORD=kontra


.PHONY : all
all:
	cabal install

# Make "pretty" diagram of database model (requires postgresql-autodoc and graphwiz)
dist/dbmodel/$(DBNAME).png:
	mkdir -p dist/dbmodel
	cd dist/dbmodel && postgresql_autodoc -t dot -d $(DBNAME) -u $(DBUSER) --password=$(DBPASSWORD)
	dot -Tpng dist/dbmodel/$(DBNAME).dot -o dist/dbmodel/$(DBNAME).png
