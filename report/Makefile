# Makefile for the Haskell Report
# read the README file before you start!
#
#	Target 'jfp' copies stuff into $(JFP_DIR) for the JFP book
#	you need to do the same in the libraries directory

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
# EXPAND = c:/cygwin/bin/expand


#########################################
#	Files 
#########################################

PREFIX = ../haskell98-revised
RELEASE_DIR = $(PREFIX)
PS_DIR = $(RELEASE_DIR)
PDF_DIR = $(RELEASE_DIR)
HTML_DIR = $(RELEASE_DIR)/html
JFP_DIR = ../jfp-release

PARTS =		preface-jfp.tex $(PARTS_NO_PREFACE)
PARTS_NO_PREFACE = iso-chars.tex \
		intro.tex lexemes.tex  exps.tex \
		decls.tex  modules.tex  basic.tex  io-13.tex \
		derived.tex \
		index-extra.tex index-intro.tex layout.tex \
		literate.tex pragmas.tex standard-prelude.tex \
		ratio.tex complex.tex ix.tex \
                numeric.tex \
                array.tex io.tex char.tex monad.tex list.tex \
                system.tex directory.tex \
                maybe.tex time.tex cputime.tex random.tex \
		locale.tex 

SYNTAX =	syntax-lexical.tex syntax-iso.tex

CODE =		lib-code/Ratio.tex lib-code/Complex.tex lib-code/Ix.tex \
                lib-code/Numeric.tex \
                lib-code/Array.tex lib-code/Char.tex lib-code/List.tex \
                lib-code/Monad.tex lib-code/Maybe.tex lib-code/IO.tex \
		lib-code/Time.tex lib-code/Locale.tex

HEADERS =	lib-hdrs/Ratio.tex lib-hdrs/Complex.tex lib-hdrs/Ix.tex \
                lib-hdrs/Numeric.tex \
                lib-hdrs/Array.tex lib-hdrs/IO.tex lib-hdrs/Char.tex \
		lib-hdrs/List.tex lib-hdrs/List1.tex \
                lib-hdrs/Monad.tex lib-hdrs/System.tex lib-hdrs/Directory.tex \
                lib-hdrs/Maybe.tex lib-hdrs/IO1.tex lib-hdrs/Random.tex \
		lib-hdrs/Time.tex lib-hdrs/Time1.tex lib-hdrs/CPUTime.tex \
		lib-hdrs/Locale.tex 

PRELUDE =	Prelude.tex PreludeList.tex PreludeText.tex PreludeIO.tex

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

# I have no idea why run_tex is run twice.  SLPJ
html: index.html haskell.dvi ${PARTS} ${SYNTAX} ${PRELUDE}
	-mkdir haskell98-report-html
	$(RUN_TEX)
	$(RUN_TEX)
	$(RUN_INDEX)
	cp index.html *.gif haskell98-report-html

haskell.dvi: haskell.tex haskell.ind ${PARTS} $(SYNTAX) ${PRELUDE}  ${CODE} ${HEADERS}


# remove this rule if you don't have "makeindex"
haskell.ind: haskell.idx
	$(MAKEINDEX) -i -t haskell.ilg < haskell.idx > haskell.ind

haskell.ps : haskell.dvi
	dvips haskell.dvi -o haskell.ps

# Can't use pdflatex because of eps inclusion
haskell.pdf : haskell.ps
	ps2pdf haskell.ps haskell.pdf

install: haskell.ps haskell.pdf html
	mkdir -p $(PS_DIR) $(PDF_DIR) $(HTML_DIR)
	cp haskell.ps $(PS_DIR)/haskell98-report.ps
	cp haskell.pdf $(PDF_DIR)/haskell98-report.pdf
	cp -r haskell98-report-html $(HTML_DIR)
	cp h98-revised.html $(HTML_DIR)/index.html
	cp haskell98-bugs.html h98.gif $(HTML_DIR)
	cp haskell98-revised-bugs.html $(HTML_DIR)


release: haskell.ps html $(RELEASE_DIR)
	ps2pdf haskell.ps
	gzip < haskell.ps > $(RELEASE_DIR)/haskell.ps.gz
	cp haskell.pdf $(RELEASE_DIR)/haskell.pdf
	cp -r haskell98-report-html $(RELEASE_DIR)
	tar cvf - haskell98-report-html | gzip > $(RELEASE_DIR)/haskell98-report-html.tar.gz
	cp h98-revised.html $(RELEASE_DIR)/index.html
	cp haskell98-bugs.html h98.gif $(RELEASE_DIR)
	cp haskell98-revised-bugs.html h98.gif $(RELEASE_DIR)

$(RELEASE_DIR):
	mkdir $@

publish-pdf: report.pdf
	gzip < report.pdf > y:Haskell/haskell98-report/report.pdf.gz


#########################################
#	Generic stuff
#########################################

veryclean: clean
	$(RM) *~ 

clean:
	$(RM) $(PRELUDE) reportrefs
	$(RM) *.dvi *.log *.aux *.ilg *.blg *.toc ${PARTS} $(SYNTAX) \
		haskell.tex haskell.ps haskell.pdf *.o lex.yy.c haskell.ind
	$(RM) -r haskell98-report-html/
	$(RM) lib-code/*.tex lib-hdrs/*.tex


#########################################
#	Suffix rules
#########################################

.SUFFIXES:	.hi .hs .verb .tex .dvi

.verb.tex:
	$(EXPAND) < $< | $(VERBATIM) | ../tools/subsection >$@
# The 'subsection' part changes section -> chapter for the book style

.hs.verb:
	$(EXPAND) < $< | $(SPLITPGM) >$@

%.tex: %.hs
	$(EXPAND) < $< | $(SPLITPGM) | $(VERBATIM) >$@

.hi.tex:
	$(EXPAND) < $< | $(SPLITPGM) | $(VERBATIM) >$@

.tex.dvi:
	$(LATEX) $<
	$(LATEX) $<
	$(LATEX) $<


# -----------------------------------------------------------
#			Out of date stuff
# -----------------------------------------------------------

# Report as formatted for SIGPLAN -- 2 sided, 2-up, odd pages on left.
# This is a bit horrible.  But almost nobody should need to do this. KH
sigplan_haskell.dvi: sigplan_haskell.verb sigplan_article.sty \
	             haskell.tex haskell.ind \
		     ${PARTS} $(SYNTAX) ${PRELUDE}
	cp sigplan_article.sty article.sty
	cp sigplan_haskell.verb haskell.verb
	-make haskell.dvi
	cp plain_haskell.verb haskell.verb


sigplan_haskell-libraries.dvi:  sigplan_haskell-libraries.tex haskell-libraries.ind ${LIBPARTS} ${LIBS}

haskell-libraries.dvi:  haskell-libraries.tex haskell-libraries.ind ${LIBPARTS} ${LIBS}

# ----------------------- END OF OLD SIGPLAN STUFF -----------------------------------

jfp: h98-book.tex preface-jfp.tex ${PARTS_NO_PREFACE} $(SYNTAX) ${PRELUDE}
	-mkdir $(JFP_DIR)
	for n in h98-book.tex preface-jfp.tex \
		${PARTS_NO_PREFACE} $(SYNTAX) ${PRELUDE} ; do  \
		../tools/subsection < $$n >  $(JFP_DIR)/$$n ; done 
	cp classes.eps $(JFP_DIR)
	cp  haskell.bbl $(JFP_DIR)/h98-book.bbl
	cp Makefile-jfp-book $(JFP_DIR)/Makefile




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

