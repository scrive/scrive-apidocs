
CLASSPATH=.:itextpdf-5.4.2.jar:itext-asian.jar:snakeyaml-1.12.jar


all : run


run : PDFSeal.class
	java -cp $(CLASSPATH) PDFSeal example_spec.json

%.class : %.java
	javac -target 1.5 -cp $(CLASSPATH) PDFSeal.java

PDFSeal.class : PDFSeal.java
