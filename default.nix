with (import ./.nix/nixpkgs.nix {});

stdenv.mkDerivation rec {
  pname = "haskell-report";
  version = "2020";

  src = ./.;

  buildInputs = [
    (texlive.combine {
      inherit (texlive)
        scheme-basic
        tex4ht
        tabulary
        collection-fontsrecommended;
    })
    flex
    ghc
    perl528
    ghostscript
  ];

  buildCommand = ''
    cp -r ${src} haskell-report
    chmod -R u+w haskell-report

    cd haskell-report
    export HOME=$TMPDIR

    make

    mkdir $out
    cp report/haskell.pdf $out/haskell-report.pdf
    cp -r report/ht $out/ht
  '';
}
