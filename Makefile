KERNEL = kernel/house

MKBE2GBF = ./mkbe2gbf

TOP	= $(shell pwd)
GHCVER  = 6.8.2
GHCSRC  = ghc-$(GHCVER)-src.tar.bz2
GHCXSRC = ghc-$(GHCVER)-src-extralibs.tar.bz2
GHCMD5  = 43108417594be7eba0918c459e871e40
GHCXMD5 = d199c50814188fb77355d41058b8613c
GHCURL  = http://www.haskell.org/ghc/dist/$(GHCVER)
GHCTOP	= $(TOP)/ghc-$(GHCVER)
GRUBDIR	= /boot/grub/
MPOINT	= $(TOP)/floppy_dir
# impractical, but stuff is too big for 1440kB floppies. :/
FLOPPYSIZE=2880

all:
	@$(MAKE) -C support
	@$(MAKE) $(KERNEL)
	@echo
	@echo "Kernel built successfully."
	@echo
	@echo "Enter 'make floppy' now to build a $(FLOPPYSIZE)KB floppy disk, suitable for loading in emulators such as bochs and qemu."
	@echo
	@echo "Enter 'make cdrom' now to build an ISO9660 image, which can be written on a CD-R to boot on a PC, and can also be used in an emulator.  Note that this requires mkisofs to be installed."
	@echo

# Helper function for downloading and checksumming. $(1) = filename, $(2) = md5sum.
download = @if [ ! -f "$(1)" ]; then\
	    echo "Downloading $(1)...";\
	    wget "$(GHCURL)/$(1)";\
	    echo -n "Testing checksum of $(1)...";\
	    if echo "$(2)  $(1)" | md5sum -c > /dev/null; then\
	      echo "OK";\
	    else\
	      echo "failed!";\
	      echo "You will need to fix this problem yourself, sorry.";\
	      exit 1;\
	    fi\
	  fi

boot:
	$(call download,$(GHCSRC),$(GHCMD5))
	$(call download,$(GHCXSRC),$(GHCXMD5))
	@if [ -e $(GHCTOP) ] ; then \
	  echo "The old directory containing the patched GHC:" ;\
	  echo "	$(GHCTOP)" ;\
	  echo "will be deleted;  press Return to continue, or Ctrl-C to abort." ;\
	  read ;\
	  rm -rf $(GHCTOP) ;\
	fi
	@echo "Unpacking $(GHCSRC)..."
	@tar --get --bzip2 --file $(GHCSRC)
	@echo "Unpacking $(GHCXSRC)..."
	@tar --get --bzip2 --file $(GHCXSRC)
	@echo "Done.  Now please do 'make'."

stamp-patch:
	@if [ ! -d $(GHCTOP) ]; then \
		echo "No 'ghc-6.8.2' directory found."; \
		echo "Please use 'make boot' to download and unpack GHC 6.8.2"; \
		echo "(or do it yourself if you have the sources handy)."; \
		exit 1; \
	fi
	@if [ ! -f patches/halvm-patches.tar.gz ]; then\
		echo "No 'patches/halvm-patches.tar.gz' file found."; \
		exit 1; \
	fi
	@if [ ! -d $(GHCTOP)/rts/house ]; then\
		rm -rf patches/halvm;\
		tar xzf patches/halvm-patches.tar.gz;\
		mv halvm-patches patches/halvm;\
		cd $(GHCTOP);\
		echo "Applying halvm patches...";\
		for patch in $(TOP)/patches/halvm/*; do sed 's/xen_HOST_OS/house_HOST_OS/g' < $$patch | patch -p1; done;\
		echo "Applying house patches...";\
		for patch in $(TOP)/patches/house/*; do patch -p1 < $$patch; done;\
		cp -pr $(TOP)/patches/new/rts/house rts/;\
	fi
	touch $@

stamp-configure: build.mk stamp-patch
	cp build.mk $(GHCTOP)/mk;\
	cd $(GHCTOP) && autoreconf && ./configure --build=i386-unknown-house
	touch $@

stamp-ghc: stamp-configure
	cd $(GHCTOP) && $(MAKE) stage1
	touch $@

$(KERNEL): stamp-ghc .phony
	$(MAKE) -C kernel

G=$(MPOINT)/boot/grub
F=$(MPOINT)/fonts.hf
P=$(MPOINT)/pci.ids.gz

floppy: House.flp
	@echo
	@echo "The floppy image is called 'House.flp'."

cdrom: House.iso
	@echo
	@echo "The cdrom image is called 'House.iso'."

House.flp: $G/stage2 kernel/house $G/grub.conf $F $P
	gzip -9 < kernel/house >$(MPOINT)/boot/kernel
	$(MKBE2GBF) $(MPOINT) $@ $(FLOPPYSIZE) $(GRUBDIR)

osker.flp: $G/stage2 kernel/osker $G/grub.conf $F $P
	gzip -9 < kernel/osker >$(MPOINT)/boot/kernel
	$(MKBE2GBF) $(MPOINT) $@ $(FLOPPYSIZE) $(GRUBDIR)

kernel/osker: ghc-stamp .phony
	$(MAKE) -C kernel osker

$G/grub.conf: menu.lst $(MPOINT)
	mkdir -p $G
	-ln -s grub.conf $(MPOINT)/boot/grub/menu.lst
	cp menu.lst $(MPOINT)/boot/grub/grub.conf

$(MPOINT):
	mkdir $(MPOINT)

$G/stage2: stage2
	mkdir -p $G
	cp stage2 $(MPOINT)/boot/grub

U=kernel/Util
$F: createFontFile.hs $U/FixedFont.hs $U/FontEncode.hs
	LANG=C runhugs -h1000000 -Pkernel: createFontFile.hs >$@

$P:
	mkdir -p $(MPOINT)
	cd $(MPOINT) && wget http://pciids.sourceforge.net/pci.ids.gz

stage2:
	wget http://www.cse.ogi.edu/~hallgren/House/stage2

House.iso: House.flp
	rm -rf iso
	mkdir iso
	ln House.flp iso/
	mkisofs -r -b House.flp -c boot.catalog -o $@ iso

clean:
	rm -rf $(MPOINT) iso
	$(MAKE) -C kernel clean

distclean:
	$(MAKE) -C support clean
	rm -rf ghc-$(GHCVER) $(GHCSRC) $(GHCXSRC) $(GHCTOP)/stamp patches/halvm

.phony:
	# dummy target to force rebuilding

