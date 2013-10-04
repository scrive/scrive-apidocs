VERSION=0.3.4

.PHONY : all
all:
	javac -target 1.5 -classpath ../../PdfVerifier-$(VERSION).jar $$(find . -name '*.java')
	$(MAKE) makejar TOOL=PdfSigner
	$(MAKE) makejar TOOL=PdfExtender
	$(MAKE) makejar TOOL=PdfVerifier
	rm $$(find . -name '*.class')

makejar:
	cp ../../$(TOOL)-$(VERSION).jar ../../$(TOOL).jar
	jar uf ../../$(TOOL).jar $$(find . -name '*.class')
