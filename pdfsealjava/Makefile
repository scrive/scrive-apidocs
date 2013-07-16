
CLASSPATH=.:itextpdf-5.4.2.jar:itext-asian.jar:snakeyaml-1.12.jar


all : output.pdf


output.pdf :  pdfseal.jar example_spec.json
	java -jar $< example_spec.json

%.class : %.java
	javac -target 1.5 -cp $(CLASSPATH) PDFSeal.java

PDFSeal.class : PDFSeal.java

open : output.pdf
	open $+

jar : pdfseal.jar

pdfseal.jar : PDFSeal.class Manifest.txt Helvetica.ttf kochi-mincho-subst.ttf sealmarker.pdf
	jar cfm $@ Manifest.txt *.class Helvetica.ttf kochi-mincho-subst.ttf sealmarker.pdf
