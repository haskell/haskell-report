# Makefile for the Haskell Report
# read the README file before you start!
#
#	Target 'jfp' copies stuff into $(JFP_DIR) for the JFP book
#	you need to do the same in the libraries directory

##########################################
# What to make if you type 'make'

default: pdf html

.PHONY: pdf
pdf : haskell.pdf

# Begin by saying
#	touch haskell.idx

#########################################
#	Tools you need
#########################################

# Stuf from the tools/directory
RUN_INDEX = ../tools/run_index
VERBATIM  = ../tools/verbatim
VERB_TEX4HT  = ../tools/verb-tex4ht

# splitAndIndexPgm won't work unless you have "perl"
SPLITPGM = perl ../tools/splitAndIndexPgm

RM    = rm -f
CP    = cp
LN    = ln -sf
LATEX = latex
PDFLATEX = pdflatex
BIBTEX = bibtex
MAKEINDEX = makeindex

# 'expand' expands tabs to spaces
# On my machine the windows program (which does something
# quite different) shadows it.  Sigh.
EXPAND = expand
# EXPAND = c:/cygwin/bin/expand


#########################################
#	Files 
#########################################

RELEASE_DIR = ../haskell-prime-draft
JFP_DIR = ../jfp-release

PARTS =		haskell.tex preface.tex $(PARTS_NO_PREFACE) $(LIB_TEX)

PARTS_NO_PREFACE = iso-chars.tex \
		intro.tex lexemes.tex  exps.tex \
		decls.tex  modules.tex  basic.tex  io-13.tex \
		derived.tex \
		index-extra.tex index-intro.tex fixity.tex \
		pragmas.tex standard-prelude.tex \
		syntax-lexical.tex syntax-iso.tex ffi.tex \
		Prelude.tex PreludeList.tex PreludeText.tex PreludeIO.tex


LIB_MODULES = Data.Array \
	Data.Char \
	Data.Complex \
	System.IO \
	System.IO.Error \
	Data.Ix \
	Data.List \
	Data.Maybe \
	Control.Monad \
	Data.Ratio \
	Numeric \
	System.Environment \
	System.Exit \
	Data.Int \
	Data.Word \
	Data.Bits \
	Foreign \
	Foreign.Ptr \
	Foreign.ForeignPtr \
	Foreign.StablePtr \
	Foreign.Storable \
	Foreign.C \
	Foreign.C.Error \
	Foreign.C.String \
	Foreign.C.Types \
	Foreign.Marshal \
	Foreign.Marshal.Alloc \
	Foreign.Marshal.Array \
	Foreign.Marshal.Error \
	Foreign.Marshal.Utils

HADDOCK_ODIR = libs

LIB_TEX = $(patsubst %, $(HADDOCK_ODIR)/%, $(addsuffix .tex, $(subst .,-,$(LIB_MODULES))))

HT_TEXS = $(patsubst %, ht/%, $(PARTS))

show:
	@echo '$(VALUE)="$($(VALUE))"'

#########################################
#	Main targets
#########################################

# The normally formatted report -- 2 sided, one-up
# haskell.verb: plain_haskell.verb
# 	-cp plain_haskell.verb haskell.verb

haskell.tex: haskell.verb
	$(EXPAND) < $< | $(VERBATIM) >$@

haskell.dvi: haskell.tex haskell.ind ${PARTS}
haskell.pdf: haskell.tex haskell.ind haddock.sty ${PARTS}

haskell.ps : haskell.dvi
	dvips haskell.dvi -o haskell.ps

# Can't use pdflatex because of eps inclusion

release: haskell.ps html
	if ! test -d $(RELEASE_DIR); then mkdir $(RELEASE_DIR); fi
	ps2pdf haskell.ps
	gzip < haskell.ps > $(RELEASE_DIR)/haskell.ps.gz
	cp haskell.pdf $(RELEASE_DIR)/haskell.pdf
	cp -r haskell-report-html $(RELEASE_DIR)
	tar cvf - haskell-report-html | gzip > $(RELEASE_DIR)/haskell-report-html.tar.gz
	cp *.png *.gif $(RELEASE_DIR)
	#cp h98-revised.html $(RELEASE_DIR)/index.html
	#cp haskell98-bugs.html h98.gif $(RELEASE_DIR)
	#cp haskell98-revised-bugs.html h98.gif $(RELEASE_DIR)

publish-pdf: report.pdf
	gzip < report.pdf > y:Haskell/haskell-report/report.pdf.gz

#########################################
#	HTML
#########################################

ht/classes.eps : classes.eps
	$(CP) $< $@

.PHONY: html
html: $(HT_TEXS) ht/classes.eps ht/haddock.sty
	cd ht && htlatex haskell.tex "haskell,2"

#########################################
#	Generic stuff
#########################################

.PHONY: clean veryclean
veryclean: clean
	$(RM) *~ $(LIB_TEX)

clean:
	$(RM) ${HT_TEXS} ht/*.html ht/html/* ht/*.htm ht/hask*.png ht/*.log ht/*.tmp \
	 	ht/*.xref ht/classes.eps ht/*.css ht/*.idx ht/*.aux ht/*.dvi \
		ht/*.4ct ht/*.4tc
	$(RM) *.dvi *.log *.aux *.ilg *.blg *.toc *.bbl *.idx *.tmp *.out \
		haskell.tex preface.tex $(PARTS_NO_PREFACE) \
		haskell.tex haskell.ps haskell.pdf *.o lex.yy.c haskell-report-html/* \
		libs/haddock.sty libs/Prelude.tex libs/haskell*.tex

# NB. don't clean $(LIB_TEX) here


#########################################
#	Suffix rules
#########################################

%.tex: %.verb
	$(EXPAND) < $< | $(VERBATIM) | sh ../tools/subsection >$@
# The 'subsection' part changes section -> chapter for the book style

%.verb: %.hs
	$(EXPAND) < $< | $(SPLITPGM) >$@

%.tex: %.hs
	$(EXPAND) < $< | $(SPLITPGM) | $(VERBATIM) >$@

%.tex: %.hi
	$(EXPAND) < $< | $(SPLITPGM) | $(VERBATIM) >$@

%.dvi: %.tex
	$(LATEX) $<

%.pdf: %.tex
	$(PDFLATEX) $<
	$(BIBTEX) $*
	$(MAKEINDEX) -i -t haskell.ilg < haskell.idx > haskell.ind
	$(PDFLATEX) $<
	$(PDFLATEX) $<

ht/%.tex: %.verb
	$(EXPAND) < $< | $(VERB_TEX4HT) | sh ../tools/subsection >$@

ht/libs/%.tex: libs/%.tex
	$(CP) $< $@

jfp: h98-book.tex preface-jfp.tex ${PARTS_NO_PREFACE}
	-mkdir $(JFP_DIR)
	for n in h98-book.tex preface-jfp.tex \
		${PARTS_NO_PREFACE}; do  \
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


# -----------------------------------------------------------------------------
# Haddock-generated library docs

# The results of auto-generating the library docs are in the
# repository already, so you only need to re-generate if you are
# changing anything.

# To generate the library docs, we need to run Haddock against the
# modules of the haskell2010 package.  The package is here:
#
#   http://darcs.haskell.org/packages/haskell2010
#
# I generated the documentation using a GHC build, but you could
# probably do it using just the package sources, as long as you have
# GHC and all the package documentation installed.
#
# Basically the plan is to run Haddock in the same way as for
# generating the HTML documentation, except that we
#
#    * replace --html with --latex
#    * Add --latex-style=NONE to prevent Haddock dumping its haddock.sty
#    * Add --odir=$(HADDOCK_DIR)
#
# So to get started, in a GHC build, you could run 
#
#    $ cd libraries/haskell2010
#    $ make html stage=0 FAST=YES
#
# And grab the Haddock command line that the build system emits.
#
# The following rules will probably need a lot of manual modification,
# sorry about that.

# Set this to Haddock (version 2.8 or later with the LaTeX back-end)
HADDOCK = /home/simonmar/builds/testing/inplace/bin/haddock

# Set this to the location of the libraries in a GHC build
LIBDIR = /home/simonmar/builds/testing/libraries

haddock :
	$(HADDOCK) --odir=$(HADDOCK_ODIR) --no-tmp-comp-dir \
	   --latex --latex-style=NONE \
	   --title="haskell2010-1.0.0.0: Compatibility with Haskell 2010" \
	   --read-interface=../ghc-prim,$(LIBDIR)/ghc-prim/dist-install/doc/html/ghc-prim/ghc-prim.haddock \
	   --read-interface=../array-0.3.0.0,$(LIBDIR)/array/dist-install/doc/html/array/array.haddock \
	   --read-interface=../base-4.3.0.0,$(LIBDIR)/base/dist-install/doc/html/base/base.haddock \
	   --optghc=-package-name --optghc=haskell2010-1.0.0.0 --optghc=-hide-all-packages --optghc=-i --optghc=-i$(LIBDIR)/haskell2010/. --optghc=-i$(LIBDIR)/haskell2010/dist-install/build --optghc=-i$(LIBDIR)/haskell2010/dist-install/build/autogen --optghc=-i$(LIBDIR)/haskell2010/dist-install/build --optghc=-i$(LIBDIR)/haskell2010/dist-install/build/autogen --optghc=-i$(LIBDIR)/haskell2010/. --optghc=-optP-include --optghc=-optP$(LIBDIR)/haskell2010/dist-install/build/autogen/cabal_macros.h --optghc=-package --optghc=array-0.3.0.0 --optghc=-package --optghc=base-4.3.0.0 --optghc=-package-name --optghc=haskell2010 --optghc=-XPackageImports --optghc=-XCPP --optghc=-no-user-package-conf --optghc=-rtsopts --optghc=-O --optghc=-fasm --optghc=-dcore-lint --optghc=-fno-warn-deprecated-flags --optghc=-odir --optghc=libraries/haskell2010/dist-install/build --optghc=-hidir --optghc=$(LIBDIR)/haskell2010/dist-install/build --optghc=-stubdir --optghc=$(LIBDIR)/haskell2010/dist-install/build --optghc=-hisuf --optghc=hi --optghc=-osuf --optghc=o --optghc=-hcsuf --optghc=hc \
	   $(patsubst %, $(LIBDIR)/haskell2010/%, $(addsuffix .hs, $(subst .,/,$(LIB_MODULES))))
	for i in $(LIB_TEX); do \
	   sed 's/instance\(\\ [^\]*\)*\\ ReadP\($$\|\\\\\)//g' $$i | \
	   sed 's/instance\(\\ [^\]*\)*\\ P\($$\|\\\\\)//g' | \
	   sed 's/instance\\ Typeable\(1\|\)\(\\ [^\]*\)*\($$\|\\\\\)//g' | \
	   sed 's/instance\\ Exception\(\\ [^\]*\)*\($$\|\\\\\)//g' |  \
	   sed 's/instance\(\\ [^\]*\)*\\ \(WordPtr\|IntPtr\)\($$\|\\\\\)//g' |  \
	   sed 's/instance\\ (Data\(\\ [^\]*\)*\($$\|\\\\\)//g' >$$i.tmp \
	   && mv $$i.tmp $$i; done

# NB. Haddock accumulates all instances including some for types and
# classes that are not part of the Haskell standard, hence we remove
# them manually here.  This seemed easier than trying to modify
# Haddock to avoid generating them somehow.
