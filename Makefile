all:  funion

funion: Funion.hs
	ghc --make $< -o $@ -threaded
#	sudo sh -c "runhaskell Funion.hs ./here; ls -l here; fusermount -u here"

run: funion
	sudo sh -c "./funion -o allow_other  here/ +/dvds/ +/disk2/dvds/"

kill: 
	sudo umount here; \
	sudo killall -9 funion
	

clean:
	rm funion Funion.hi Funion.o DirTools/DirTools.o DirTools/DirTools.hi
