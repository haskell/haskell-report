RELEASE_DIR = haskell-prime-draft
JFP_DIR = jfp-release

default:
	cd tools && make
	cd report && make

release:
	(cd tools; make)
	(cd report; make release)
	(cd libraries; make release)
	(cd jfp-release; make)
	cp report/haskell-prime-draft.html $(RELEASE_DIR)/index.html
	cp hprime.png $(RELEASE_DIR)
	gzip < jfp-release/h98-book.ps > $(RELEASE_DIR)/h98-book.ps.gz
	gzip < jfp-release/h98-book.pdf > $(RELEASE_DIR)/h98-book.pdf.gz

jfp:
	-mkdir $(JFP_DIR)
	(cd report; make jfp)
	(cd libraries; make jfp)

# Places to change when you change the date of the Report
# 	h98-revised.html
#	report/index.html   libraries/index.html
#	report/html.config  libraries/html.config
#	report/haskell.verb libraries/library.verb
