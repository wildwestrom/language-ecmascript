with (import ./default.nix {});
with pkgs.haskell.lib;
let
  tarball = sdistTarball language-ecmascript;
in
pkgs.writeScriptBin "upload" ''
  ${pkgs.cabal-install}/bin/cabal upload ${tarball}/*.tar.gz --publish
''
