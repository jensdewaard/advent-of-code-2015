main: main.hs
	ghc main.hs


.PHONY: clean
clean:
	@rm -f *.hi *.o main