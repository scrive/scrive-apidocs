
CLASSPATH=.:itextpdf-5.4.2.jar:itext-asian.jar:snakeyaml-1.12.jar


all : output.pdf


../test/pdfs/simple.pdf-javasealed.pdf :  pdfseal.jar example_spec.json ../test/pdfs/simple.pdf
	java -jar $< example_spec.json

%.class : %.java
	javac -target 1.5 -cp $(CLASSPATH) PDFSeal.java

PDFSeal.class : PDFSeal.java

open : ../test/pdfs/simple.pdf-javasealed.pdf
	open $+

jar : pdfseal.jar

pdfseal.jar : PDFSeal.class Manifest.txt Helvetica.ttf
	jar cfm $@ Manifest.txt *.class Helvetica.ttf
