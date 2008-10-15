# OCaml-Mad examples.
#
# Copyright 2003 by the Savonet team.
#
# $Id$

all clean:
	$(MAKE) -f Makefile.ogg2wav $@
	$(MAKE) -f Makefile.wav2ogg $@
	$(MAKE) -f Makefile.stream2wav $@

distclean: clean
	rm -rf autom4te.cache config.log config.status autom4te.cache
	rm -f Makefile.wav2ogg Makefile.stream2wav
