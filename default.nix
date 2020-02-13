with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
let
  haskellOverlay = self: super: with super.haskell.lib; {
    cabal2nix = (self.haskellPackages.override {
      overrides = self: super: {
        network = dontCheck (super.network);
        network_3_1_1_1 = dontCheck (super.network_3_1_1_1);
      };
    }).cabal2nix;
  };
in
{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { overlays = [ ]; }
, compiler ? "ghc881"
}:
let
  overrides = with pkgs.haskell.lib;
    self: super: {
      Diff = self.Diff_0_4_0;
    };
  ghc = pkgs.haskell.packages.${compiler}.override { inherit overrides; };
  language-ecmascript = ghc.callCabal2nix "language-ecmascript" ./. {};
in
  language-ecmascript
