with (import ./default.nix {});
with pkgs.haskell.lib;
buildFromSdist language-ecmascript

