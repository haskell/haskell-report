
RELEASE_DIR = haskell98-revised

release:
	(cd report; make release)
	(cd libraries; make release)
	cp h98-revised.html $(RELEASE_DIR)/index.html
	cp haskell98-bugs.html h98.gif $(RELEASE_DIR)
