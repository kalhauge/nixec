{ mkDerivation, base, bytestring, cassava, directory, filepath
, free, hpack, lens, mtl, prettyprinter, stdenv, text
, typed-process
}:
mkDerivation {
  pname = "nixec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava directory filepath free lens mtl
    prettyprinter text typed-process
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cassava directory filepath free lens mtl
    prettyprinter text typed-process
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/kalhauge/nixec#readme";
  description = "A system for specifying evaluation scripts";
  license = stdenv.lib.licenses.bsd3;
}
