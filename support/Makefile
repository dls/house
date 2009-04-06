TARGETS = libtiny_c.a libtiny_gcc.a libtiny_m.a libtiny_gmp.a

all: 
	make -C c all
	make -C gcc all
	make -C m all
	make -C gmp all

clean:
	rm -rf $(TARGETS)
	make -C c clean
	make -C gcc clean
	make -C m clean
	make -C gmp clean
