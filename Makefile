# OCaml-Vorbis
#
# Copyright (c) 2003 by the Savonet team
#
# $Id$

PROGNAME = ocaml-vorbis
DISTFILES = aclocal.m4 bootstrap CHANGES configure configure.ac \
            COPYING Makefile README m4/*.m4 \
            src/OCamlMakefile* src/Makefile.in \
            src/META.in src/*.ml src/*.mli src/*.c \
            examples/*Makefile* examples/*.ml 

all:

all opt byte install uninstall update:
	$(MAKE) -C src $@

doc:
	$(MAKE) -C src htdoc
	mkdir -p doc
	rm -rf doc/html
	mv src/doc/vorbis/html doc
	rm -rf src/doc

examples:
	$(MAKE) -C examples

clean:
	-$(MAKE) -C src clean
	-$(MAKE) -C examples clean

distclean: clean
	rm -rf autom4te.cache config.log config.status src/META src/Makefile
	rm -rf doc
	-$(MAKE) -C examples distclean

dist: doc
	VERSION="$(shell grep 'AC_INIT' configure.ac)"; \
		VERSION=`echo "$$VERSION" | sed -e 's/AC_INIT([^,]*, \([^,]*\), .*)/\1/'`; \
		mkdir $(PROGNAME)-$$VERSION; \
		cp -r --parents $(DISTFILES) $(PROGNAME)-$$VERSION; \
		tar zcvf $(PROGNAME)-$$VERSION.tar.gz $(PROGNAME)-$$VERSION; \
		rm -rf $(PROGNAME)-$$VERSION

.PHONY: doc all byte opt install uninstall update clean distclean dist examples
