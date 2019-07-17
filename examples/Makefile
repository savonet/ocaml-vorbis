all clean:
	$(MAKE) -f Makefile.ogg2wav $@
	$(MAKE) -f Makefile.wav2ogg $@
	$(MAKE) -f Makefile.stream2wav $@

test: all
	if [ ! -e test.wav ] ; then ffmpeg -f lavfi -i "sine=frequency=440:duration=5" -ac 2 test.wav ; fi
	./wav2ogg test.wav test.ogg
	./ogg2wav test.ogg test2.wav
	./stream2wav test.ogg test3.wav
