{ mkDerivation, base, lens, stdenv }:
mkDerivation {
  pname = "chorebot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base lens ];
  description = "A bot to distribute chores";
  license = stdenv.lib.licenses.gpl2;
}
