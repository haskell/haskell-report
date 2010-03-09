all : hier.ps hier.pdf

hier.ps: hier.tex
	TEXINPUTS=../styles: latex $<
	dvips -f < hier.dvi > $@

hier.pdf: hier.tex
	TEXINPUTS=../styles: pdflatex $<

.PHONY: clean
clean:
	-rm -f *.aux *.log *.blg *.dvi *.bbl *.toc *.ps *.css *.png *.4ct *.4tc *.idv *.xref tex4ht.fls *.pdf
