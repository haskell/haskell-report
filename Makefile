RELEASE_DIR = haskell98-revised
JFP_DIR = jfp-release

release:
	(cd report; make release)
	(cd libraries; make release)
	(cd jfp-release; make)
	cp h98-revised.html $(RELEASE_DIR)/index.html
	cp haskell98-bugs.html h98.gif $(RELEASE_DIR)
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
