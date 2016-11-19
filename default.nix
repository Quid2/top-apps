{ pkgs ? import <nixpkgs> {} }:

let
hsLib = pkgs.haskell.lib;
haskellPackages = pkgs.haskell.packages.ghc7103.override {
  overrides = self: super: {
    flat = hsLib.dontHaddock (self.callPackage (import ./flat.nix) {});
    model = hsLib.dontHaddock (self.callPackage (import ./model.nix) {});
    top = hsLib.dontHaddock (self.callPackage (import ./top.nix) {});
    top-apps = hsLib.dontHaddock (self.callPackage (import ./top-apps.nix) {});
    typed = hsLib.dontHaddock (self.callPackage (import ./typed.nix) {});
  };
};
drv = haskellPackages.top-apps;

in if pkgs.lib.inNixShell then drv.env else drv
