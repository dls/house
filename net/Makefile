
OPT=-O
O=objs$(OPT)
GHCFLAGS = $(OPT) -fglasgow-exts -fallow-undecidable-instances -i../kernel -odir $O -hidir $O -optl -s

all:: $O opentun.o
	ghc --make $(GHCFLAGS) tst.hs -o tst opentun.o

objs: ; mkdir objs
objs-O: ; mkdir objs-O

opentun.o: opentun.c opentun.h

clean:
	rm -r objs tst opentun.o
