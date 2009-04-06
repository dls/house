SRCS = dlmalloc.c libc.c wctype.c bsearch.c fakeerrno.c
OBJS = $(patsubst %.c,%.o,$(SRCS))

all: $(OBJS)
	ar rc ../libtiny_c.a *.o
	ranlib ../libtiny_c.a

clean:
	rm -rf *.o *~

%.o: %.c
	gcc -DSTANDALONE -O3 -fomit-frame-pointer -c $< -o $@
