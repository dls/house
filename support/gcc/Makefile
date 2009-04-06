OBJS = _divdi3.oS _moddi3.oS _udivdi3.oS _umoddi3.oS

all:
	ar x $(shell gcc -print-libgcc-file-name) $(OBJS)
	ar rc ../libtiny_gcc.a $(OBJS)
	ranlib ../libtiny_gcc.a

clean:
	rm -f *~
