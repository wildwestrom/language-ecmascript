with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) {}
, compiler ? "ghc881"
}:
let
  overrides = self: super: { Diff = self.Diff_0_4_0; };
  ghc = pkgs.haskell.packages.${compiler}.override { inherit overrides; };
  language-ecmascript = ghc.callCabal2nix "language-ecmascript" ./. {};
in
with pkgs.haskell.lib;
{
  inherit language-ecmascript;
  inherit pkgs;
  release = sdistTarball language-ecmascript;
}
