TARGET = ../libtiny_gmp.a
OBJS = assert.o memory.o

EXC_OBJS = \
	assert.o memory.o set_str.o dump.o inp_str.o out_str.o		\
	dump.o fac_ui.o inp_raw.o inp_str.o out_raw.o out_str.o dump.o	\
	sscanffuns.o vfscanf.o vscanf.o randlc.o set_d.o inp_str.o	\
	out_str.o set_str.o asprntffuns.o doprnt.o doprntf.o doprnti.o	\
	obprntffuns.o printf.o printffuns.o sprintf.o snprntffuns.o	\
	vasprintf.o vfprintf.o vsnprintf.o vsprintf.o doscan.o		\
	fscanf.o fscanffuns.o scanf.o set_d.o set_str.o sprintffuns.o	\
	vprintf.o sscanf.o vsscanf.o set_str.o

all: $(OBJS)
	cp /usr/lib/libgmp.a $(TARGET)
	ar d $(TARGET) $(EXC_OBJS)
	ar r $(TARGET) $(OBJS)
	ranlib $(TARGET)

clean:
	rm -f $(OBJS) *~

%.o: %.c
	gcc -DSTANDALONE -O3 -fomit-frame-pointer -c $< -o $@
