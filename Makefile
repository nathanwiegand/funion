MOUNTPOINT=union

all:  funion

funion: Funion.hs
	ghc --make $< -o $@ -threaded
#	sudo sh -c "runhaskell Funion.hs ./here; ls -l here; fusermount -u here"

run: funion
	if [ ! -d $(MOUNTPOINT) ]; then mkdir $(MOUNTPOINT); fi
	sudo sh -c "./funion -o allow_other  $(MOUNTPOINT)/ +/dvds/ +/disk2/dvds/"

kill: 
	sudo umount $(MOUNTPOINT); \
	sudo killall -9 funion; \
	rmdir $(MOUNTPOINT)

lint:
	clear; hlint -c Funion.hs | head -n 20
	

clean:
	rm -f funion Funion.hi Funion.o DirTools/DirTools.o DirTools/DirTools.hi
