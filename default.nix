{ mkDerivation, base, bytestring, cassava, containers, data-fix
, directory, dirtree, filepath, free, hnix, hpack, lens, mtl
, optparse-applicative, prettyprinter, process, stdenv, text
, transformers, vector
}:
mkDerivation {
  pname = "nixec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava containers data-fix directory dirtree
    filepath free hnix lens mtl optparse-applicative prettyprinter
    process text transformers vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cassava containers data-fix directory dirtree
    filepath free hnix lens mtl optparse-applicative prettyprinter
    process text transformers vector
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/kalhauge/nixec#readme";
  description = "A system for specifying evaluation scripts";
  license = stdenv.lib.licenses.bsd3;
}
