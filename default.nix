{ mkDerivation, base, containers, extra, parsec, stdenv, time }:
mkDerivation {
  pname = "chorebot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers extra parsec time ];
  description = "A bot to distribute chores";
  license = stdenv.lib.licenses.gpl2;
}
