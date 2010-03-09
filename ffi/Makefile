
all: ffi.ps ffi.pdf

threads.ps: threads.tex
	TEXINPUTS=../styles: latex threads.tex
	dvips -f < threads.dvi > threads.ps

ffi.pdf: ffi.ps
	ps2pdf ffi.ps ffi.pdf

ffi.ps: ffi.tex
	TEXINPUTS=../styles: latex ffi.tex
	bibtex ffi
	TEXINPUTS=../styles: latex ffi.tex
	TEXINPUTS=../styles: latex ffi.tex
	dvips -f < ffi.dvi > ffi.ps

# Requires TeX4ht
ffi.html: ffi.tex ffi.cfg
	xhlatex ffi.tex "ffi,2"

# This is yucky, but there is not much choice as we need to subsitute a 
# different Makefile for the tarball.
#
TMP=/tmp
.PHONY: ffi.tar.gz
ffi.tar.gz:
	mkdir $(TMP)/ffi
	cp ffi.tex $(TMP)/ffi/
	cp ffi.bib $(TMP)/ffi/
	cp ffi.cfg $(TMP)/ffi/
	cp ../styles/grammar.sty $(TMP)/ffi/
	cp COPYING.ffi $(TMP)/ffi/COPYING
	cp Makefile.ffi $(TMP)/ffi/Makefile
	cd $(TMP); tar -czf $(PWD)/ffi.tar.gz ffi
	-rm -f $(TMP)/ffi/*
	-rmdir $(TMP)/ffi

TMP=/tmp
.PHONY: ffi.html.tar.gz
ffi.html.tar.gz:
	mkdir $(TMP)/ffi
	cp *.html *.png ffi.css $(TMP)/ffi/
	cp COPYING.ffi $(TMP)/ffi/COPYING
	cd $(TMP); tar -czf $(PWD)/ffi.html.tar.gz ffi
	-rm -f $(TMP)/ffi/*
	-rmdir $(TMP)/ffi

.PHONY: clean
clean:
	-rm -f *.aux *.log *.blg *.dvi *.bbl *.toc *.ps *.html *.css *.png *.4ct *.4tc *.idv *.xref tex4ht.fls *.pdf
