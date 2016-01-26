{ mkDerivation, base, containers, directory, extra, hspec, parsec
, random, regex-compat, stdenv, time
}:
mkDerivation {
  pname = "chorebot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory extra parsec random regex-compat time
  ];
  executableHaskellDepends = [ base directory random time ];
  testHaskellDepends = [ base hspec ];
  description = "A bot to distribute chores";
  license = stdenv.lib.licenses.gpl2;
}
