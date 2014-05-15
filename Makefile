CSC = csc

ALL: chipmunk chipmunk.so chipmunk.import.so

FORCE:

chipmunk: FORCE
	cd deps/chipmunk; $(MAKE)

chipmunk.so: chipmunk.scm vect_hacks.c binding-helpers.scm
	$(CSC) -s -O3 -d1 deps/chipmunk/chipmunk.a chipmunk.scm -j chipmunk

chipmunk.import.so: chipmunk.import.scm
	$(CSC) -s -O3 -d0 chipmunk.import.scm

clean:
	$(RM) -f chipmunk.so chipmunk.import.so chipmunk-high.so chipmunk-high.import.so

clean_all: clean
	cd deps/chipmunk; $(MAKE) clean
