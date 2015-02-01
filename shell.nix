{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "HaskellEmbedded";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [ hakyll ];
  buildTools = with haskellPackages; [ cabalInstall ];
  meta = {
    description = "HaskellEmbedded web page stuff.";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
