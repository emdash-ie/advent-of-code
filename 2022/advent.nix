{ mkDerivation, base, containers, data-fix, generic-lens, lens, lib
, text
}:
mkDerivation {
  pname = "advent";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers data-fix generic-lens lens text
  ];
  description = "Advent of code 2021";
  license = lib.licenses.gpl3Plus;
}
