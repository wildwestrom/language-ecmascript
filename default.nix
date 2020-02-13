{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc881"
}:
let
  overrides = self: super: { Diff = super.Diff_0_4_0; };
  ghc = pkgs.haskell.packages.${compiler}.override { inherit overrides; };
  language-ecmascript = ghc.callCabal2nix "language-ecmascript" ./. {};
in
  language-ecmascript
