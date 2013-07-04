
CLASSPATH=.:itextpdf-5.4.2.jar:itext-asian.jar:snakeyaml-1.12.jar


all : run


run : AppendMode.class
	java -cp $(CLASSPATH) AppendMode

%.class : %.java
	javac -target 1.5 -cp $(CLASSPATH) AppendMode.java

AppendMode.class : AppendMode.java
