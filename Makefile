main: *.hs
	ghc --make main.hs


.PHONY: clean
clean:
	@rm -f *.hi *.o main