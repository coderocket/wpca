
all : AlloyCmdLine.class Output

Output:
	make -C Output

AlloyCmdLine.class : AlloyCmdLine.java
#	javac -cp ".:/Applications/Alloy4.2.app/Contents/Resources/Java/alloy-dev.jar" AlloyCmdLine.java
	javac -cp ".;../../alloy4.1.10/alloy4.jar" AlloyCmdLine.java
	cp *.class ../

# javac -cp ".;../../alloy4.1.10/alloy4.jar" AlloyCmdLine.java
# javac -cp ".:/Applications/Alloy4.app/Contents/Resources/Java/alloy4.jar" AlloyCmdLine.java

# alloy command: java -cp ".;../alloy4.1.10/alloy4.jar" AlloyCmdLine analysis.als

.PHONY : all Output

