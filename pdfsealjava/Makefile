
CLASSPATH1=itext-asian.jar:snakeyaml-1.12.jar:bcpkix-jdk15on-1.48.jar:bcprov-jdk15on-1.48.jar
CLASSPATH=.:itextpdf-5.4.2.jar:$(CLASSPATH1)

all : output.pdf


output.pdf :  pdfseal.jar itextpdf-5.4.2.jar example_spec.json
	java -jar $< example_spec.json

%.class : %.java itextpdf-5.4.2.jar
	javac -target 1.5 -cp $(CLASSPATH) PDFSeal.java

PDFSeal.class : PDFSeal.java

open : output.pdf
	open $+

jar : pdfseal.jar

pdfseal.jar : PDFSeal.class Manifest.txt sealmarker.pdf
	jar cfm $@ Manifest.txt *.class sealmarker.pdf

#
# We need to make jar dependent on all .java file in
# itextpdf-5.4.2-sources. There is one file that needs Java 1.6
# MakeXmlSignature.java and we want to keep Java 1.5 compatibility, at
# least for now.
ITEXTPDF_SOURCES=$(shell find itextpdf-5.4.2-sources -name '*.java' -not -name MakeXmlSignature.java)
itextpdf-5.4.2.jar : $(ITEXTPDF_SOURCES)
	@-mkdir itextpdf-5.4.2 2> /dev/null
	echo Sources:
	echo $(ITEXTPDF_SOURCES)
	javac -target 1.5 -cp $(CLASSPATH1) $(ITEXTPDF_SOURCES) -d itextpdf-5.4.2
	jar cf $@ -C itextpdf-5.4.2 com


test-pdfs/incorrect-ID-length-output.pdf : test-pdfs/incorrect-ID-length.json test-pdfs/incorrect-ID-length.pdf pdfseal.jar itextpdf-5.4.2.jar
	java -jar pdfseal.jar $<
	open $@

test-pdfs/incorrect-ID-length.json : test-pdfs/incorrect-ID-length.pdf example_spec.json
	echo '{"input": "'$<'",' > $@
	echo ' "output": "test-pdfs/incorrect-ID-length-output.pdf",' >> $@
	tail -n +3 example_spec.json >> $@
	head -n 10 $@

test-pdfs/broken-fonts-output.pdf : test-pdfs/broken-fonts.json test-pdfs/broken-fonts.pdf pdfseal.jar itextpdf-5.4.2.jar
	java -jar pdfseal.jar $<
	open $@

test-pdfs/broken-fonts.json : test-pdfs/broken-fonts.pdf example_spec.json
	echo '{"input": "'$<'",' > $@
	echo ' "output": "test-pdfs/broken-fonts-output.pdf",' >> $@
	tail -n +3 example_spec.json >> $@
	head -n 10 $@

test-pdfs/acro-forms-empty-output.pdf : test-pdfs/acro-forms-empty.json test-pdfs/acro-forms-empty.pdf pdfseal.jar itextpdf-5.4.2.jar
	java -jar pdfseal.jar $<
	open $@

test-pdfs/acro-forms-empty.json : test-pdfs/acro-forms-empty.pdf example_spec.json
	echo '{"input": "'$<'",' > $@
	echo ' "output": "test-pdfs/acro-forms-empty-output.pdf",' >> $@
	tail -n +3 example_spec.json >> $@
	head -n 10 $@


test-pdfs/acro-forms-filled-in-output.pdf : test-pdfs/acro-forms-filled-in.json test-pdfs/acro-forms-filled-in.pdf pdfseal.jar itextpdf-5.4.2.jar
	java -jar pdfseal.jar $<
	open $@

test-pdfs/acro-forms-filled-in.json : test-pdfs/acro-forms-filled-in.pdf example_spec.json
	echo '{"input": "'$<'",' > $@
	echo ' "output": "test-pdfs/acro-forms-filled-in-output.pdf",' >> $@
	tail -n +3 example_spec.json >> $@
	head -n 10 $@
