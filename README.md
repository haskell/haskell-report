Haskell Report
==============

These are the sources to the Haskell report, including all the source files you
will need to generate either the PDF or the HTML version of the report.


Building via Nix
----------------

To build using the [Nix](https://nixos.org/nix/) package manager, simply run
`nix-build`. The PDF and HTML versions can then be found in the `result`
subdirectory.

Alternatively, you can invoke `nix-shell` to enter a shell with the environment
needed to build the report. Once in the shell you can use `make` as normal.

Building manually
-----------------

### Tools you will need

PDF version: a decent LaTeX installation with pdflatex.  We use the following
additional packages:

  - times
  - makeidx
  - graphicx
  - url
  - color
  - hyperref

Also you need the following tools

  - makeindex

all of which are usually available with a good TeX distribution (e.g. TeX Live).

The following are also required for building the tools:

  - flex
  - GHC

The HTML version additionally requires

  - tex4ht (e.g. install `tex4ht` on a Debian or Ubuntu system, or
    `tetex-tex4ht` on a Fedora system)

### Building

Firstly:

    $ cd tools
    $ make

should build a few tools required for building the report itself. Then you
should be able to say

    $ cd report
    $ make

This will create:

   - PDF version: report/haskell.pdf

   - HTML version: report/ht/haskell.html
     (NB. requires report/ht/*.{html,png,css})


Roadmap
-------

Source files

  - `report/` The Language and Libraries Reports (now together in a single document)
  - `tools/` Tools needed to build the Reports (cd into here and type make)
  - `Makefile` Build a distribution of the Reports
