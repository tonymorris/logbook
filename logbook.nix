{ mkDerivation, base, checkers, deriving-compat, digit, HTTP, lens
, network-uri, QuickCheck, semigroupoids, semigroups, stdenv
, tagsoup, tagsoup-selection, tasty, tasty-hunit, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "logbook";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base deriving-compat digit HTTP lens network-uri semigroupoids
    semigroups tagsoup tagsoup-selection transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/tonymorris/casr-logbook";
  description = "Australian Pilot logbook";
  license = stdenv.lib.licenses.bsd3;
}
