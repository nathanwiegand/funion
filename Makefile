SHELL=bash

all:  funion

funion: Funion.hs
	ghc --make $< -o $@ -threaded

lint:
	clear; hlint -c Funion.hs | head -n 20

clean:
	rm -f funion Funion.hi Funion.o ; 
