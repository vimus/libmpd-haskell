{}:
with import <nixpkgs>{};

let
  haskell = haskellPackages;
  inherit (haskell) cabal cabalInstall dataDefault filepath mtl attoparsec
  network text time utf8String;
in

cabal.mkDerivation (self: {
  pname = "libmpd";
  version = "0.9.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  propagatedDependencies = [ cabalInstall ];
  buildDepends = [
    attoparsec dataDefault filepath mtl network text time utf8String
  ];
  doCheck = false;
})
