all : hier.ps hier.pdf

hier.ps: hier.tex
	TEXINPUTS=../styles: latex $<
	dvips -f < hier.dvi > $@

hier.pdf: hier.tex
	TEXINPUTS=../styles: pdflatex $<

clean :
	rm hier.ps hier.pdf
