# Makefile for the Haskell Report
# read the README file before you start!

##########################################
# What to make if you type 'make'

default: haskell.ps html

# Begin by saying
#	touch haskell.idx


#########################################
#	Tools you need
#########################################

# Stuf from the tools/directory
RUN_TEX   = ../tools/run_tex
RUN_INDEX = ../tools/run_index
VERBATIM  = ../tools/verbatim

# splitAndIndexPgm won't work unless you have "perl"
SPLITPGM = perl ../tools/splitAndIndexPgm

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

PARTS =		preface-13.tex iso-chars.tex \
		intro.tex lexemes.tex  exps.tex \
		decls.tex  modules.tex  basic.tex  io-13.tex \
		derived.tex \
		index-extra.tex index-intro.tex layout.tex \
		literate.tex pragmas.tex standard-prelude.tex
SYNTAX =	syntax-lexical.tex syntax-iso.tex
PRELUDE =	Prelude.tex PreludeList.tex \
		PreludeText.tex PreludeIO.tex

LIBPARTS =	lib-array.tex lib-intro.tex lib-io.tex lib-num.tex lib-os.tex lib-text.tex \
		lib-index-intro.tex lib-posix.tex

LIBS =		LibMonadicIO.tex LibStdIO.tex LibIOError.tex LibReadTextIO.tex LibWriteTextIO.tex \
		LibSystem.tex LibDirectory.tex LibCPUTime.tex LibTime.tex LibUserInterrupt.tex \
		LibRatio.tex LibArray.tex LibComplex.tex LibText.tex \
		LibIO.tex LibIx.tex LibPosix.tex


#########################################
#	Main targets
#########################################

# The normally formatted report -- 2 sided, one-up
haskell.verb: plain_haskell.verb
	-cp plain_haskell.verb haskell.verb

haskell.tex: haskell.verb
	$(EXPAND) < $< | $(VERBATIM) >$@

haskell.dvi:  haskell.tex ${PARTS} $(SYNTAX) ${PRELUDE}

# Report as formatted for SIGPLAN -- 2 sided, 2-up, odd pages on left.
# This is a bit horrible.  But almost nobody should need to do this. KH
sigplan_haskell.dvi: sigplan_haskell.verb sigplan_article.sty \
	             haskell.tex haskell.ind \
		     ${PARTS} $(SYNTAX) ${PRELUDE}
	cp sigplan_article.sty article.sty
	cp sigplan_haskell.verb haskell.verb
	-make haskell.dvi
	cp plain_haskell.verb haskell.verb


# I have no idea why run_tex is run twice.  SLPJ
html: index.html ${PARTS} ${SYNTAX} ${PRELUDE}
	-mkdir haskell98-report-html
	$(RUN_TEX)
	$(RUN_TEX)
	$(RUN_INDEX)
	cp index.html *.gif haskell98-report-html

haskell.dvi: haskell.tex haskell.ind ${PARTS} $(SYNTAX) ${PRELUDE}

haskell-libraries.dvi:  haskell-libraries.tex haskell-libraries.ind ${LIBPARTS} ${LIBS}

sigplan_haskell-libraries.dvi:  sigplan_haskell-libraries.tex haskell-libraries.ind ${LIBPARTS} ${LIBS}

# remove this rule if you don't have "makeindex"
haskell.ind: haskell.idx
	$(MAKEINDEX) -i -t haskell.ilg < haskell.idx > haskell.ind

veryclean: clean
	$(RM) *~ 

clean:
	$(RM) *.dvi *.log *.aux *.ilg *.blg *.toc ${PARTS} $(SYNTAX) \
		haskell.tex haskell.ps haskell.pdf *.o lex.yy.c haskell98-report-html/*
	cp haskell.ind haskell.ind.two-sided
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

haskell-2up.ps : haskell-2up.dvi
	dvips -t landscape -x 707 haskell-2up.dvi > haskell-2up.ps

haskell-2up.dvi : haskellx.dvi
	-dvidvi '2:0(0in,0in),1(160mm,0in)' haskellx.dvi > haskell-2up.dvi

haskellx.dvi : sigplan_haskell.dvi
	dviselect =4: haskell.dvi haskellx.dvi

haskell-libraries-2up.ps : haskell-libraries-2up.dvi
	dvips -t landscape -x 707 haskell-libraries-2up.dvi > haskell-libraries-2up.ps

haskell-libraries-2up.dvi : haskell-librariesx.dvi
	-dvidvi '2:0(0in,0in),1(160mm,0in)' haskell-librariesx.dvi > haskell-libraries-2up.dvi

haskell-librariesx.dvi : sigplan_haskell-libraries.dvi
	dviselect =4: sigplan_haskell-libraries.dvi haskell-librariesx.dvi

haskell.ps : haskell.dvi
	dvips haskell.dvi -o haskell.ps



publish: haskell.ps html
	gzip < haskell.ps > y:Haskell/haskell98-report/haskell.ps.gz
	cp haskell98-report-html/* y:Haskell/haskell98-report
	tar cvf - haskell98-report-html | gzip > y:Haskell/haskell98-report/haskell98-report-html.tar.gz

publish-pdf: report.pdf
	gzip < report.pdf > y:Haskell/haskell98-report/report.pdf.gz


#########################################
#	Suffix rules
#########################################

.SUFFIXES:	.hi .hs .verb .tex .dvi

.verb.tex:
	$(EXPAND) < $< | $(VERBATIM) >$@

.hs.verb:
	$(EXPAND) < $< | $(SPLITPGM) >$@

%.tex: %.hs
	$(EXPAND) < $< | $(SPLITPGM) | $(VERBATIM) >$@

.hi.tex:
	$(EXPAND) < $< | $(SPLITPGM) | $(VERBATIM) >$@

.tex.dvi:
	$(LATEX) $<

