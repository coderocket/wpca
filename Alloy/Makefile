
all : AlloyCmdLine.class AnalysisOutput

AnalysisOutput:
	make -C AnalysisOutput

AlloyCmdLine.class : AlloyCmdLine.java
	javac -cp ".:../../alloy4/alloy4.jar" AlloyCmdLine.java

# javac -cp ".;../../alloy4.1.10/alloy4.jar" AlloyCmdLine.java
# javac -cp ".:/Applications/Alloy4.app/Contents/Resources/Java/alloy4.jar" AlloyCmdLine.java

# alloy command: java -cp ".;../alloy4.1.10/alloy4.jar" AlloyCmdLine analysis.als

.PHONY : all AnalysisOutput

