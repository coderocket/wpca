
.PHONY: clean

wpca.exe : cfg.hs cfgtoks.hs c.hs alloy.hs main.hs tokens.hs grammar.hs ast.hs wp.hs 
	ghc cfg.hs cfgtoks.hs c.hs alloy.hs main.hs ast.hs wp.hs tokens.hs grammar.hs -o wpca.exe

tokens.hs : tokens.x
	alex tokens.x

grammar.hs : tokens.x grammar.y
	happy grammar.y

cfgtoks.hs : cfgtoks.x
	alex cfgtoks.x

cfg.hs : cfgtoks.x cfg.y
	happy cfg.y

clean:
	rm cfg.o cfg.hs tokens.hs grammar.hs cfgtoks.hs cfgtoks.o c.o alloy.o main.o tokens.o grammar.o ast.o wp.o wpca.exe

