{ mkDerivation, base, bytestring, cassava, containers, directory
, filepath, free, hpack, lens, mtl, optparse-applicative
, prettyprinter, stdenv, text, transformers, typed-process
}:
mkDerivation {
  pname = "nixec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava containers directory filepath free lens mtl
    optparse-applicative prettyprinter text transformers typed-process
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cassava containers directory filepath free lens mtl
    optparse-applicative prettyprinter text transformers typed-process
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/kalhauge/nixec#readme";
  description = "A system for specifying evaluation scripts";
  license = stdenv.lib.licenses.bsd3;
}
