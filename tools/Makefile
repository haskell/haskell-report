#	Tools for building the Haskell Report

# tex.hs             -- Program to generate html from verb / hs
# index.hs           -- Program to hyperlink prelude index
# splitAndIndexPgm   -- perl script to convert .hs to .verb


# What is GHC?
GHC = ghc

# What is Lex?
LEX = flex
LEX_OPTS = -t -8

all : verbatim run_tex run_index

verbatim: verbatim.lex
	$(LEX) $(LEX_OPTS) verbatim.lex > verbatim.c || ( rm -f verbatim.c && exit 1 )
	$(CC) -c verbatim.c -o verbatim.o
	$(CC) -o verbatim verbatim.o

run_tex: tex.hs
	$(GHC) -o run_tex tex.hs -fglasgow-exts -H20m -package lang

run_index: index.hs
	$(GHC) -o run_index index.hs -fglasgow-exts -H20m

