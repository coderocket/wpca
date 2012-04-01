
all : Lexer.hs Parser.hs

Lexer.hs : Lexer.x
	alex Lexer.x

Parser.hs : Parser.y
	happy Parser.y

.PHONY: clean all

clean:
	rm Lexer.hs Parser.hs

