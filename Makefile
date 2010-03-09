PREFIX = haskell98-revised
RELEASE_DIR = $(PREFIX)
JFP_DIR = jfp-release

install:
	$(MAKE) -C tools
	$(MAKE) -C report install

clean:
	$(MAKE) -C tools clean
	$(MAKE) -C report clean

release:
	(cd tools; make)
	(cd report; make release)
	touch libraries/library.idx
	(cd libraries; make release)
	cp report/h98-revised.html $(RELEASE_DIR)/index.html

jfp:
	-mkdir $(JFP_DIR)
	(cd report; make jfp)
	(cd libraries; make jfp)

# Places to change when you change the date of the Report
# 	h98-revised.html
#	report/index.html   libraries/index.html
#	report/html.config  libraries/html.config
#	report/haskell.verb libraries/library.verb
