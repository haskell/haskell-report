# Makefile for the Haskell Library Report
# read the README file before you start!
#

# Begin by saying
#	touch library.idx

default: library.ps.gz html


#########################################
#	Tools you need
#########################################

# Stuf from the tools/directory
RUN_TEX   = ../tools/run_tex
RUN_INDEX = ../tools/run_index
VERBATIM  = ../tools/verbatim

# splitAndIndexPgm won't work unless you have "perl"
SPLITPGM = perl ../tools/splitAndIndexPgm

CC    = gcc
RM    = rm -f
LATEX = latex
MAKEINDEX = makeindex

# 'expand' expands tabs to spaces
# On my machine the windows program (which does something
# quite different) shadows it.  Sigh.
EXPAND = expand


#########################################
#	Files
#########################################

# I've been having problems with that damn NT messing up filenames in the html
# generator so I'm changing the .verb files to lower case  -- jcp

RELEASE_DIR = ../haskell98-revised

PARTS =		library.tex introduction.tex ratio.tex complex.tex ix.tex \
                numeric.tex \
                array.tex io.tex char.tex monad.tex list.tex \
                system.tex directory.tex \
                maybe.tex time.tex cputime.tex random.tex \
		locale.tex 

CODE =		code/Ratio.tex code/Complex.tex code/Ix.tex \
                code/Numeric.tex \
                code/Array.tex code/Char.tex code/List.tex \
                code/Monad.tex code/Maybe.tex code/IO.tex \
		code/Time.tex code/Locale.tex

CODEV =		code/Ratio.verb code/Complex.verb code/Ix.verb \
                code/Numeric.verb \
                code/Array.verb code/Char.verb code/List.verb \
                code/Monad.verb code/Maybe.verb code/IO.verb code/Bit.verb \
		code/Time.verb code/Locale.verb

HEADERS =	headers/Ratio.tex headers/Complex.tex headers/Ix.tex \
                headers/Numeric.tex \
                headers/Array.tex headers/IO.tex headers/Char.tex \
		headers/List.tex headers/List1.tex \
                headers/Monad.tex headers/System.tex headers/Directory.tex \
                headers/Maybe.tex headers/IO1.tex headers/Random.tex \
		headers/Time.tex headers/Time1.tex headers/CPUTime.tex \
		headers/Locale.tex 

HEADERSV =	headers/Ratio.verb headers/Complex.verb headers/Ix.verb \
                headers/Numeric.verb \
                headers/Array.verb headers/IO.verb headers/Char.verb \
		headers/List.verb headers/List1.verb \
                headers/Monad.verb headers/System.verb headers/Directory.verb \
                headers/Maybe.verb headers/IO1.verb headers/Random.verb \
		headers/Time.verb headers/CPUTime.verb \
		headers/Locale.verb 

# The normally formatted report -- 2 sided, one-up
library.dvi: library.ind ${PARTS} ${CODE} ${HEADERS} index-intro.tex

html: index.html ${PARTS} ${SYNTAX} ${PRELUDE}
	-mkdir haskell98-library-html
	$(RUN_TEX)
	$(RUN_TEX)
	cp *.gif index.html haskell98-library-html

# remove this rule if you don't have "makeindex"
library.ind: library.idx
	makeindex -i -t library.ilg < library.idx > library.ind

veryclean: clean
	$(RM) *~ code/*~ headers/*~

clean:
	$(RM) *.dvi *.log *.aux *.ilg *.blg *.toc haskell.tex \
		$(PARTS) $(CODE) $(HEADERS) \
		library.ps library.pdf haskell98-library-html/*
	cp library.ind library.ind.two-sided 
	echo "Don't delete the Prelude*.tex files"
	echo "Not everyone has \"perl\" to re-make them"


# Stuff to make the "two-up" version for SIGPLAN Notices:
# We take the A4 pages and double them up onto (virtual) A3 pages:
# (two A5 pages make one A4 page; two A4 pages make one A3 page; ...)
#
#	% dvidvi '2:0,1(210mm,0)' haskell.dvi > haskell-2up.dvi
#
# We now print the "A3" pages in landscape, but "magnify" them by
# 1 / sqrt(2) ~ 0.7071; this should make our "A4" pages print in an
# A4 space: presto! 2up printing!
# (except that I've tried to make it give us 1" margins all round
#  when we print on US 8.5"x11" paper)
#
#	% dvips -t landscape -x 707 haskell-2up.dvi > haskell-2up.ps
#
# if your "dvips" is set up properly, it will run Metafont for you,
# creating just-right magnifications for all the fonts.
#
# print with: lpr -Pmy-laserwriter haskell-2up.ps

# These rules keep failing for me (segmentation fault on haskell-2up.dvi)
# The output seems fine, though, and the individual commands work
# perfectly(!).  More network wierdness?  KH


verbs:  $(CODEV) $(HEADERSV)

library.ps : library.dvi
	dvips library.dvi -o library.ps

release: library.ps html
	gzip < library.ps > $(RELEASE_DIR)/library.ps.gz
	cp -r haskell98-library-html $(RELEASE_DIR)
	tar cvf - haskell98-library-html | gzip > $(RELEASE_DIR)/haskell98-library-html.tar.gz

publish-pdf: library.pdf
	gzip < library.pdf > y:Haskell/haskell98-library/library.pdf.gz


#########################################
#	Suffix rules
#########################################

.SUFFIXES:	.hs .verb .tex .dvi .gz .ps

.verb.tex:
	expand < $< | $(VERBATIM) >$@

.hs.verb:
	expand < $< | $(SPLITPGM) >$@

.hs.tex:
	expand < $< | $(SPLITPGM) | $(VERBATIM) >$@

.tex.dvi:
	$(LATEX) $<


