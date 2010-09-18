all:
	ghc -O ../cbits/double.c ../cbits/float.c \
	    -i. -i.. Tests.hs --make -o test-ieee
	./test-ieee

hpc:
	ghc -fforce-recomp ../cbits/double.c ../cbits/float.c \
	    -i. -i.. -fhpc --make Tests.hs -o test-ieee
	rm -f test-mc.tix
	./test-ieee
	hpc markup test-ieee

clean:
	find ../lib . -name '*.hi' | xargs rm -f
	find ../lib . -name '*.o'  | xargs rm -f
	find . -name '*.html' | xargs rm -f
	rm -f test-ieee test-ieee.tix
	rm -rf .hpc
	