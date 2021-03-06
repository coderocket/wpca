
refute.exe : Config Alloy Parser.hs Lexer.hs
	ghc --make main.hs -o refute.exe

Config:
	make -C Config
Alloy:
	make -C Alloy

Parser.hs : Parser.y
	happy -i Parser.y

Lexer.hs : Lexer.x
	alex Lexer.x

.PHONY: Config Alloy clean test

clean:
	rm refute.exe

test : 1.test 2.test 3.test 4.test 5.test 6.test 7.test 8.test 9.test

%.test: refute.exe
	./refute.exe test$(@:.test=.w)
	diff -w analysis.als test$(@:.test=.expected)
	@echo passed
	
