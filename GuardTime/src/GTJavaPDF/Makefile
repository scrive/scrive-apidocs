.PHONY : all
all:
	javac -classpath ../../PdfVerifier-0.3.2-scrive.jar $$(find . -name '*.java')
	$(MAKE) makejar TOOL=PdfSigner
	$(MAKE) makejar TOOL=PdfExtender
	$(MAKE) makejar TOOL=PdfVerifier
	rm $$(find . -name '*.class')

makejar:
	cp ../../$(TOOL)-0.3.2-scrive.jar ../../$(TOOL).jar
	jar uf ../../$(TOOL).jar $$(find . -name '*.class')