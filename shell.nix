{ pkgs ? import <nixpkgs> {}
, fficxxSrc ? (import ./nix/pinned.nix { inherit pkgs; }).fficxxSrc
}:

with pkgs;

let
  # TODO: should be packaged into the upstream nixpkgs.
  DataFrame = callPackage ./DataFrame/default.nix {};

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
    };
  };

  stdcxxSrc = import (fficxxSrc + "/stdcxx-gen/gen.nix") {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
      "stdcxx"         = self.callCabal2nix "stdcxx"         stdcxxSrc                       {};
    };

  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    cabal2nix
    cabal-install
    #
    fficxx
    fficxx-runtime
    formatting
    monad-loops
    stdcxx
  ]);

in

stdenv.mkDerivation {
  name = "HDataFrame-dev";

  buildInputs = [
    DataFrame
    pkgconfig
    hsenv
  ];

}
