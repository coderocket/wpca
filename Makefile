
sources = anaOutToks.hs anaOut.hs cfg.hs cfgtoks.hs c.hs alloy.hs main.hs ast.hs wp.hs tokens.hs grammar.hs 

wpca.exe : $(sources) 
	ghc $(sources) -o wpca.exe

anaOutToks.hs : anaOutToks.x
	alex anaOutToks.x

anaOut.hs : anaOutToks.x anaOut.y
	happy anaOut.y

tokens.hs : tokens.x
	alex tokens.x

grammar.hs : tokens.x grammar.y
	happy grammar.y

cfgtoks.hs : cfgtoks.x
	alex cfgtoks.x

cfg.hs : cfgtoks.x cfg.y
	happy cfg.y

AlloyCmdLine.class : AlloyCmdLine.java
	javac -cp ".;../alloy4.1.10/alloy4.jar" AlloyCmdLine.java

# alloy command: java -cp ".;../alloy4.1.10/alloy4.jar" AlloyCmdLine analysis.als

.PHONY: clean test

clean:
	rm $(sources:.hs=.o) wpca.exe

test : 1.test 2.test 3.test 4.test 5.test 6.test 7.test 8.test 9.test

%.test: wpca.exe
	./wpca.exe test$(@:.test=.w)
	diff -w analysis.als test$(@:.test=.expected)
	@echo passed
	
