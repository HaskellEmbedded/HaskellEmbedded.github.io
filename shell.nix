/*
So far, this is the direct output of cabal2nix on the project.
Building this should just be a matter of running inside nix-shell:
cabal configure
cabal build
cabal run build

*/

{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, filepath, hakyll, stdenv }:
      mkDerivation {
        pname = "haskellembedded";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base filepath hakyll ]
          ++ stdenv.lib.optional stdenv.isDarwin
             pkgs.darwin.apple_sdk.frameworks.Cocoa;
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
