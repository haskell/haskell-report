threads.ps: threads.tex
	TEXINPUTS=../styles: latex threads.tex
	dvips -f < threads.dvi > threads.ps

ffi.ps: ffi.tex
	TEXINPUTS=../styles: latex ffi.tex
	dvips -f < ffi.dvi > ffi.ps

# This is yucky, but there is not much choice as we need to subsitute a 
# different Makefile for the tarball.
#
TMP=/tmp
.PHONY: ffi.tar.gz
ffi.tar.gz:
	mkdir $(TMP)/ffi
	cp ffi.tex $(TMP)/ffi/
	cp ffi.bib $(TMP)/ffi/
	cp ../styles/grammar.sty $(TMP)/ffi/
	cp COPYING.ffi $(TMP)/ffi/COPYING
	cp Makefile.ffi $(TMP)/ffi/Makefile
	cd $(TMP); tar -czf $(PWD)/ffi.tar.gz ffi
	-rm -f $(TMP)/ffi/*
	-rmdir $(TMP)/ffi
