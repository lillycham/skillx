{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, lib, optparse-applicative, tasty, tasty-hunit
, tasty-quickcheck, temporary, text, toml-parser
}:
mkDerivation {
  pname = "skillx";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath
    optparse-applicative text toml-parser
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath tasty
    tasty-hunit tasty-quickcheck temporary text
  ];
  homepage = "https://github.com/lillycham/skillx";
  description = "A declarative multi-harness skill graph compiler and manager";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "skillx";
}
