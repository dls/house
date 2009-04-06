# make top a symbolic link to .. or whatever you need!
TOP     = top

# No more of this!
#TOP	= ..
#TOP	= /home/apt/projects/hOp
#TOP	= /home/diatchki/src/hOp-2004-06-09

GHCTOP  = $(TOP)/ghc-6.8.2

CC	= gcc
CFLAGS	= -Wall -Wno-unused-variable -O2 -I$(GHCTOP)/includes -I$(GHCTOP)/rts -fno-builtin
#GHC     = $(GHCTOP)/compiler/stage1/ghc-inplace
GHC     =  $(GHCTOP)/compiler/stage1/ghc-6.8.2 -B$(GHCTOP)

# Optimize? (Can be used for other options too, e.g. -prof)
OPT     = -O
#OPT     = 

HOUSE   = house
OSKER   = osker
HOVEL   = hovel

LIBHSRTS = $(GHCTOP)/rts/libHSrts.a
LIBHSBASE = $(GHCTOP)/libraries/base/libHSbase.a
LIBHSBASE_CBITS = $(GHCTOP)/libraries/base/cbits/libHSbase_cbits.a

C_OBJS = cbits/start.o cbits/c_start.o cbits/gprintf.o cbits/gfx.o \
	 cbits/modules.o cbits/userspace.o cbits/fault.o cbits/util.o  \
	 cbits/io.o

# Swap comment on these lines if you don't want to use Bochs special IO port
# for debugging.
C_OBJS	+= cbits/video.o
#CFLAGS	+= -DBOCHS_VIDEO

OBJS    = $(C_OBJS) House.o $(HS_OBJS) $(G_OBJS)

all: $(HOUSE)

ODIR=objs
GHCOPTS = -W -pgmc $(CC) -odir $(ODIR) -hidir $(ODIR) -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances


HOUSE_SRC= -Icbits -iGadgets/Images -iGadgets -iGadgets/lib House.hs

$(HOUSE):: cobjs $(ODIR)
	rm -f $(HOUSE)
	$(GHC) --make -pgml ./ldhouse $(GHCOPTS) $(OPT) $(HOUSE_SRC) $(C_OBJS) -o $@
	# check if kernel respects the format of a Multiboot (GRUB) kernel
	mbchk $(HOUSE)

cobjs: $(C_OBJS)

L4HAL = L4/Interfaces/HAL

$(OSKER):: cobjs $(ODIR) $(L4HAL)
	$(GHC) --make -pgml ./ldhouse $(GHCOPTS) -main-is Osker -no-hs-main -Icbits $(OPT) L4/Osker.hs $(C_OBJS) -o $@
	# check if kernel respects the format of a Multiboot (GRUB) kernel
	# mbchk $(OSKER)



$(HOVEL):: cobjs $(ODIR) 
	$(GHC) --make -pgml ./ldhouse $(GHCOPTS) -main-is Hovel -no-hs-main -Icbits $(OPT) Hovel.hs $(C_OBJS) -o $@
	# check if kernel respects the format of a Multiboot (GRUB) kernel
	# mbchk $(HOVEL)


$(L4HAL) :
	ln -s H L4/Interfaces/HAL

#$(LIBHSRTS):
#	make -C $(GHCTOP)/rts all

#$(LIBHSBASE) $(LIBHSBASE_CBITS):
#	make -C $(GHCTOP)/libraries/base boot
#	make -C $(GHCTOP)/libraries/base all

$(ODIR):
	mkdir $(ODIR)

################################################################################

clean:
	rm -fr $(ODIR)
	rm -f $(HOUSE) $(OSKER) $(L4HAL)
	@make -C user clean
	rm -f cbits/*.o

oldclean:
	find . -type f -name '*~' -exec rm '{}' ';'
	find . -type f -name '*.o' -exec rm '{}' ';'
	find . -type f -name '*.hi' -exec rm '{}' ';'
	rm -f $(HOUSE)
	rm -f $(OSKER)

################################################################################

pfesetup::
	pfesetup +h $(HOUSE_SRC)

hdoc::
	@mkdir -p Hdoc
	haddock -h -o Hdoc -t "Hardware Monad Interface and other modules from House (work in progress)" H/*.hs Util/*.hs Monad/*.hs Net/*.hs

html::
	@make --no-print-directory hi/srclist.txt
	pfe htmlfiles

hi/srclist.txt:
	@make --no-print-directory pfesetup

######### Perhaps this belongs in cbits/Makefile ###############################

cbits/video.o: cbits/video.c cbits/video.h
	gcc $(CFLAGS) -o $@ -c $<

cbits/util.o: cbits/util.c cbits/util.h
	gcc $(CFLAGS) -o $@ -c $<

cbits/c_start.o: cbits/c_start.c cbits/start.h cbits/config.h cbits/util.h cbits/ia32.h cbits/video.h cbits/vbe.h cbits/gfx.h cbits/modules.h cbits/multiboot.h cbits/segments.h cbits/fault.h cbits/userspace.h cbits/mib.h cbits/timer.h 
cbits/userspace.o : cbits/userspace.c cbits/userspace.h cbits/ia32.h cbits/config.h cbits/util.h cbits/segments.h cbits/fault.h 
cbits/gfx.o: cbits/gfx.c cbits/gfx.h cbits/vbe.h
cbits/modules.o: cbits/modules.c cbits/modules.h cbits/multiboot.h
cbits/fault.o: cbits/fault.c cbits/fault.h cbits/util.h
cbits/gprintf.o: cbits/gprintf.c 
