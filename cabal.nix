{ mkDerivation, aeson, async, base, data-default, directory
, filepath, lib, mtl, optics-core, profunctors, relude, shower
, time, with-utf8
}:
mkDerivation {
  pname = "haskell-template";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base data-default directory filepath mtl optics-core
    profunctors relude shower time with-utf8
  ];
  homepage = "https://srid.ca/haskell-template";
  description = "A template for Haskell projects using Nix";
  license = lib.licenses.mit;
  mainProgram = "haskell-template";
}
