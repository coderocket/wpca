
sources = cfg.hs cfgtoks.hs c.hs alloy.hs main.hs ast.hs wp.hs tokens.hs grammar.hs 

wpca.exe : $(sources) 
	ghc $(sources) -o wpca.exe

tokens.hs : tokens.x
	alex tokens.x

grammar.hs : tokens.x grammar.y
	happy grammar.y

cfgtoks.hs : cfgtoks.x
	alex cfgtoks.x

cfg.hs : cfgtoks.x cfg.y
	happy cfg.y

.PHONY: clean test

clean:
	rm $(sources:.hs=.o) wpca.exe

test : 1.test 2.test 3.test 4.test 5.test 6.test 7.test 8.test 9.test

%.test: wpca.exe
	./wpca.exe test$(@:.test=.w)
	diff analysis.als test$(@:.test=.expected)
	@echo passed
	
