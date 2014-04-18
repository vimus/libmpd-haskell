{ haskellPackages ? (import <nixpkgs>{}).haskellPackages_ghc782.profiling }:
with haskellPackages; cabal.mkDerivation (self: rec {
  pname = "libmpd";
  version = "0.9.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  propagatedDependencies = [ cabalInstall ];
  buildDepends = [
    attoparsec dataDefault filepath mtl network text time utf8String
  ];
  testDepends = buildDepends ++ [ QuickCheck hspec ];
  doCheck = true;
})
