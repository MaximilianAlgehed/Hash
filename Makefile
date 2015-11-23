all:
	ghc -threaded Hash.hs

clean:
	rm *.o *.hi Hash
