{ ghcWithPackages, runCommand }:

let
  ghc = ghcWithPackages (ps: with ps; [
    attoparsec
    text
  ]);

in
runCommand "sanitize-blocklist" { } ''
  ${ghc}/bin/ghc -Wall -Werror ${./Main.hs} -o $out
''
