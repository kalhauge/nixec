{ mkDerivation, base, bytestring, cassava, containers, directory
, filepath, free, hpack, lens, mtl, optparse-applicative
, prettyprinter, process, stdenv, text, transformers
}:
mkDerivation {
  pname = "nixec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava containers directory filepath free lens mtl
    optparse-applicative prettyprinter process text transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cassava containers directory filepath free lens mtl
    optparse-applicative prettyprinter process text transformers
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/kalhauge/nixec#readme";
  description = "A system for specifying evaluation scripts";
  license = stdenv.lib.licenses.bsd3;
}
