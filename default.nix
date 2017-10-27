with import <nixpkgs> {};

stdenv.mkDerivation {
    name = "haskell-report";
    buildInputs =
        [ (texlive.combine{ inherit (texlive)
                            scheme-basic
                            tex4ht
                            tabulary
                            collection-fontsrecommended
                            ;})
          flex
          ghc
          perl
          ghostscript
        ];
    buildCommand = ''
        cp -r ${./.} haskell-report
        chmod -R u+w haskell-report
        cd haskell-report

        export HOME=$TMPDIR

        make

        mkdir $out
        cp report/haskell.pdf "$out/haskell-report.pdf"
        cp -r report/ht "$out/haskell-report-html"
    '';
}
