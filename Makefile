
wpca.exe : Config Alloy 
	ghc --make main.hs -o wpca.exe

Config:
	make -C Config
Alloy:
	make -C Alloy

.PHONY: Config Alloy clean test

clean:
	rm wpca.exe

test : 1.test 2.test 3.test 4.test 5.test 6.test 7.test 8.test 9.test

%.test: wpca.exe
	./wpca.exe test$(@:.test=.w)
	diff -w analysis.als test$(@:.test=.expected)
	@echo passed
	
